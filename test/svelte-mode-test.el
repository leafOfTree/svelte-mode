;;; svelte-mode-test.el --- Tests for svelte-mode -*- lexical-binding:t -*-

;;; Commentary:

;; Run with `make test', or
;;   emacs -Q --batch -L . -l test/svelte-mode-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:
(require 'ert)
(require 'svelte-mode)

(defmacro svelte-test-with-buffer (content &rest body)
  "Run BODY in a `svelte-mode' buffer with CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (svelte-mode)
     (setq indent-tabs-mode nil)
     (syntax-propertize (point-max))
     (goto-char (point-min))
     ,@body))

(defun svelte-test-goto (string)
  "Move point to the start of STRING."
  (goto-char (point-min))
  (search-forward string)
  (goto-char (match-beginning 0)))

(defun svelte-test-sexp-at (string)
  "Return the sexp starting at STRING."
  (svelte-test-goto string)
  (buffer-substring-no-properties (point) (scan-lists (point) 1 0)))

(defun svelte-test-indent (content)
  "Return CONTENT indented by `svelte-mode'."
  (svelte-test-with-buffer content
    (indent-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defconst svelte-test-component
  "<script>
  const f = (a) => a > 1 && a < 3;
</script>

<a
  href={link.href}
  onmouseenter={() => open(i)}
  onfocus={() => x > 1 && y < 2}
  style={{ a: `${x > 1}` }}
>
  <span class=\"text-lg\">{link.label}</span>
  {#if a > b && c < d}<b>x</b>{/if}
</a>
"
  "A component with expressions containing `<' and `>'.")

;;; Expressions (#16)

(ert-deftest svelte-test-expression-brackets ()
  "`<' and `>' in expressions are not brackets."
  (svelte-test-with-buffer svelte-test-component
    (should (equal (svelte-test-sexp-at "{() => open")
                   "{() => open(i)}"))
    (should (equal (svelte-test-sexp-at "{() => x")
                   "{() => x > 1 && y < 2}"))
    (should (equal (svelte-test-sexp-at "{{ a:")
                   "{{ a: `${x > 1}` }}"))
    (should (equal (svelte-test-sexp-at "{#if a >")
                   "{#if a > b && c < d}"))))

(ert-deftest svelte-test-expression-tags ()
  "Tags with expressions can be skipped over."
  (svelte-test-with-buffer svelte-test-component
    (svelte-test-goto "<a\n")
    (sgml-skip-tag-forward 1)
    (should (looking-back "</a>" (line-beginning-position)))
    (svelte-test-goto "<span")
    (sgml-skip-tag-forward 1)
    (should (looking-back "</span>" (line-beginning-position)))))

(ert-deftest svelte-test-expression-submodes ()
  "`>' in <script> and <style> uses the submode syntax."
  (svelte-test-with-buffer "<script>
  const f = (a) => a > 1;
</script>
<style>
  .a > .b { color: red; }
</style>
"
    (svelte-test-goto "a > 1")
    (forward-char 2)
    (should (equal (svelte--submode-name
                    (get-text-property (point) 'svelte-submode))
                   "JavaScript"))
    (svelte-test-goto ".a > .b")
    (forward-char 3)
    (should (equal (svelte--submode-name
                    (get-text-property (point) 'svelte-submode))
                   "CSS"))))

(ert-deftest svelte-test-expression-edit ()
  "Editing inside an expression keeps `>' as punctuation."
  (svelte-test-with-buffer "<p>hello</p>
<a
  onmouseenter={() =>
    open(i)}
  class=\"x\"
>link</a>
"
    (svelte-test-goto "open(i)")
    (forward-char (length "open(i)"))
    (insert " && y > 0")
    ;; Propertize from the edit only, as jit-lock does
    (syntax-propertize (point-max))
    (search-backward ">")
    (should (eq (syntax-class (syntax-after (point))) 1))
    (should (equal (svelte-test-sexp-at "{()")
                   "{() =>\n    open(i) && y > 0}"))))

(ert-deftest svelte-test-expression-large-file ()
  "Expressions across propertize chunks in a large file."
  (let ((block "<button on:click={() => {
count += 1;
}}>
+
</button>
"))
    (svelte-test-with-buffer (apply #'concat (make-list 300 block))
      (indent-region (point-min) (point-max))
      (goto-char (point-min))
      (let ((count 0))
        (while (search-forward "on:click={" nil t)
          (save-excursion
            (backward-char)
            (should (string-suffix-p
                     "}}"
                     (buffer-substring-no-properties
                      (point) (scan-lists (point) 1 0)))))
          (setq count (1+ count)))
        (should (= count 300))))))

;;; Indentation (#11)

(ert-deftest svelte-test-indent-expression ()
  "Multi-line expressions are indented by JS rules."
  (should (equal (svelte-test-indent "<p>
{#if x}
<b>{foo({
a: 1,
})}</b>
{/if}
</p>
")
                 "<p>
  {#if x}
    <b>{foo({
      a: 1,
    })}</b>
  {/if}
</p>
")))

(ert-deftest svelte-test-indent-expression-arrow ()
  "A line after `=>' in an expression is indented by JS rules."
  ;; js-mode before Emacs 27 aligns it with the text after `{'
  (skip-unless (>= emacs-major-version 27))
  (should (equal (svelte-test-indent "<div>
<span on:click={() =>
upBox(\"vector\", {
a: \"0.2\",
show: true,
})}>
</span>
</div>
")
                 "<div>
  <span on:click={() =>
    upBox(\"vector\", {
      a: \"0.2\",
      show: true,
    })}>
  </span>
</div>
")))

(ert-deftest svelte-test-indent-component ()
  "Indent <script>, <style>, blocks and expressions."
  (should (equal (svelte-test-indent "<script>
import X from './X.svelte';
function go(a) {
if (a > 1) {
return a;
}
}
</script>

<style>
.a > .b {
color: red;
}
</style>

<main>
<h1>Hello {name}!</h1>
{#each items as item, i}
<Item {item} on:select={() => go(i)} />
{:else}
<p>empty</p>
{/each}
<button on:click={() => {
count += 1;
}}>
+
</button>
</main>
")
                 "<script>
  import X from './X.svelte';
  function go(a) {
    if (a > 1) {
      return a;
    }
  }
</script>

<style>
  .a > .b {
    color: red;
  }
</style>

<main>
  <h1>Hello {name}!</h1>
  {#each items as item, i}
    <Item {item} on:select={() => go(i)} />
  {:else}
    <p>empty</p>
  {/each}
  <button on:click={() => {
    count += 1;
  }}>
    +
  </button>
</main>
")))

;;; Imenu (#13)

(ert-deftest svelte-test-imenu ()
  "Headings without text are skipped, others may span lines."
  (svelte-test-with-buffer "<script>
  const s = \"<h2>not a heading</h2>\";
</script>

<section>
    <h1>
        <span class=\"welcome\">
        </span>
    </h1>
    <h2 class=\"t\">Simple</h2>
    <h3>
      <span>{title}</span> and
      more
    </h3>
    <!-- <h2>commented</h2> -->
    <h1><a href=\"/\">Inline <b>tag</b></a></h1>
</section>
"
    (should (equal (mapcar #'car (svelte-imenu-index))
                   '("  Simple" "    {title} and more" "Inline tag")))))

(ert-deftest svelte-test-imenu-empty ()
  "A heading without text gives an empty index."
  (svelte-test-with-buffer "<section>
    <h1>
        <span class=\"welcome\">
        </span>
    </h1>
</section>
"
    (should (null (svelte-imenu-index)))))

;;; Major mode (#12)

(ert-deftest svelte-test-major-mode ()
  "`major-mode' stays `svelte-mode', other variables are switched."
  (svelte-test-with-buffer "<script>
  let x = 1;
</script>
<style>
  .a { color: red; }
</style>
<p>x</p>
"
    (dolist (spot '(("let x" . "// ")
                    (".a {" . "/*")
                    ("<p>" . "<!-- ")))
      (svelte-test-goto (car spot))
      (svelte--pre-command)
      (should (eq major-mode 'svelte-mode))
      (should (equal comment-start (cdr spot))))))

(provide 'svelte-mode-test)
;;; svelte-mode-test.el ends here
