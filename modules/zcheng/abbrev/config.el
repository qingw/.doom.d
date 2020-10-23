;;; zcheng/abbrev/config.el -*- lexical-binding: t; -*-

(define-abbrev-table 'global-abbrev-table '(
                                            ("8spc" "spacemacs")
                                            ("8al" ";;;###autoload")
                                            ;; chinese
                                            ("8waiw" "图片外网(原图)链接")
                                            ;; math/unicode symbols
                                            ("8in" "∈")
                                            ("8nin" "∉")
                                            ("8inf" "∞")
                                            ("8luv" "♥")
                                            ("8smly" "☺")
                                            ("8me" "gccll.love@gmail.com")
                                            ("8zc" "Zhicheng Lee")
                                            ;; dev
                                            ("8html5" "
<!DOCTYPE html>
<html lang='en'>
  <head>
    <meta charset='UTF-8' />
    <meta name='viewport' content='width=device-width, initial-scale=1.0' />
    <title>Document</title>
  </head>
  <body></body>
</html>
")
                                            ("8script" "
#+begin_export html
<script>
window.g_need_fold = 1
</script>
#+end_export")
                                            ;; org prop, tags
                                            ("8cid" "
:PROPERTIES:
:COLUMNS: %CUSTOM_ID[(Custom Id)]
:CUSTOM_ID: literal_eg
:END:")
                                            ;; markdown
                                            ("8font" "<font color='red' size='2'>xx</font>")
                                            ;; html template for hugo
                                            ("8red" "@@html:<font color='red'>@@text@@html:</font>@@")
                                            ("8sup" "@@html:<sup><font color='red'>@@官方@@html:</font></sup>@@")
                                            ("8sub" "@@html:<sub><font color='red'>@@官方@@html:</font></sub>@@")
                                            ("8img" "
#+BEGIN_EXPORT html
<img src='' alt='some picture'/>
#+END_EXPORT")
                                            ;; path
                                            ("8vuecc" "https://img.cheng92.com/vue3/compiler-core/tests/")
                                            ("8plib" "/img/vue3/compiler-core/lib/")
                                            ("8pcg" "/img/vue3/compiler-core/pcg/")
                                            ("8plibo" "https://www.cheng92.com/img/vue3/compiler-core/lib/")
                                            ("8ipath" "https://gcclll.gitee.io/mind-maps/vue3/")
                                            ("8ipcc" "https://gcclll.gitee.io/mind-maps/vue3/compiler-core/")
                                            ("8kbd" "@@html:<kbd>@@text@@html:</kbd>@@")

                                            ;; vue tags
                                            ("8ptag" "/vue/vue3-source-code-compiler-core/#parsetagcontext-type-parent")
                                            ("8pbase" "/vue/vue3-source-code-compiler-core/#baseparsecontext-options")
                                            ("8pele" "/vue/vue3-source-code-compiler-core/#parseelementcontext-mode" )

                                            ;; js import
                                            ("8idelay" "import { ndelay } from '@commons/timer/delay'")
                                            ("8idis" "import dispatch from '@commons/actions/dispatch'")
                                            ("8imarker" "import marker from '@commons/requests/marker'")
                                            ("8ilang" "import lang from '@commons/langs/t'")
                                            ("8ikeys" "import keys from '@commons/keymaps/'")
                                            ("8icls" "import cls from '@commons/cclass'")
                                            ("8iback" "import goBack from '@commons/back'")
                                            ("8ife" "import fetch from '@commons/fetch'")
                                            ("8iquery" "import { queryParams as query } from '@commons/param'")
                                            ("8ireq" "import - from '@commons/requests/-'")
                                            ("8istore" "import store from '@/config/store'")
                                            ("8ievent" "import EVENT from '@commons/event'")
                                            ("8ivideo" "import videoHandler from '@commons/medias/video-handler'")
                                            ("8iscroll" "import { scroll } from '@commons/plugins/simple-scroll")

                                            ;; js
                                            ("8jlog" "console.log()")

                                            ;; css import
                                            ("81com" "@import '~@commons/styles/common';")
                                            ("81imp" "!important")
                                            ))
