#' Internal UI for the Quarto Help app
#' @noRd
quartohelp_app_ui <- function() {
  bslib::page_fillable(
    title = "Quarto Help",
    theme = bslib::bs_theme(version = 5),
    shiny::tagList(
      shiny::tags$style(
        shiny::HTML(
          "
          html, body, .bslib-page { height: 100%; }
          body, .bslib-page { margin: 0; }
          .bslib-page .main, [data-bslib-main] { padding: 1rem; }
          .content-split { display: flex; gap: 0; height: calc(100vh - 2rem); position: relative; }
          .left-pane { flex: 0 0 var(--left-pane-width, 40%); min-width: 240px; max-width: 80%; display: flex; flex-direction: column; }
          .right-pane { flex: 1 1 auto; display: flex; flex-direction: column; min-width: 20%; }
          .left-pane, .right-pane { min-height: 0; }
          .split-resizer { width: 6px; cursor: col-resize; background: transparent; position: relative; }
          .split-resizer::after { content: ''; position: absolute; top: 0; bottom: 0; left: 2px; width: 2px; background: var(--bs-border-color, #dee2e6); }
          #toggle-chat-show { display: none; }
          .collapsed-left #toggle-chat-show { display: inline-flex; }
          .card { display: flex; flex-direction: column; height: 100%; }
          .card-body { flex: 1; overflow: auto; }
          .iframe-wrap { flex: 1; }
          iframe { width: 100%; height: 100%; border: none; }
          .collapsed-left .left-pane, .collapsed-left .split-resizer { display: none !important; }
          .collapsed-left .split-reveal { display: flex !important; }
          .collapsed-left .right-pane { flex: 1 1 auto; }
          .resizing iframe { pointer-events: none !important; }
          .resizing, .resizing * { cursor: col-resize !important; }
          "
        )
      ),
      shiny::div(
        class = "content-split",
        shiny::div(
          class = "left-pane",
          bslib::card(
            bslib::card_header(
              shiny::div(
                class = "d-flex align-items-center justify-content-between gap-2",
                shiny::tags$span("Chat"),
                shiny::div(
                  class = "btn-group btn-group-sm",
                  shiny::actionButton(
                    inputId = "clear_chat",
                    label = "Clear",
                    icon = shiny::icon("broom"),
                    class = "btn-outline-secondary"
                  ),
                  shiny::tags$button(
                    id = "toggle-chat",
                    type = "button",
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Collapse chat",
                    `aria-label` = "Collapse chat",
                    shiny::icon("chevron-left")
                  )
                )
              )
            ),
            bslib::card_body(
              shiny::div(
                id = "chat-pane",
                shiny::uiOutput("chat_panel")
              )
            )
          )
        ),
        shiny::div(id = "split-resizer", class = "split-resizer"),
        shiny::div(
          class = "right-pane",
          bslib::card(
            bslib::card_header(
              shiny::div(
                class = "d-flex align-items-center justify-content-between gap-3",
                shiny::div(
                  class = "d-flex align-items-center gap-2",
                  shiny::tags$button(
                    id = "toggle-chat-show",
                    type = "button",
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Show chat",
                    `aria-label` = "Show chat",
                    shiny::icon("chevron-right")
                  ),
                  shiny::tags$button(
                    id = "iframe-back",
                    type = "button",
                    class = "btn btn-sm btn-secondary",
                    title = "Back",
                    `aria-label` = "Back",
                    shiny::HTML("&#8592;")
                  ),
                  shiny::tags$button(
                    id = "iframe-forward",
                    type = "button",
                    class = "btn btn-sm btn-secondary",
                    title = "Forward",
                    `aria-label` = "Forward",
                    shiny::HTML("&#8594;")
                  ),
                  shiny::tags$span("Docs")
                ),
                shiny::div(
                  class = "d-flex align-items-center gap-3",
                  shiny::tags$button(
                    id = "open-preview-external",
                    type = "button",
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Open in new tab",
                    `aria-label` = "Open in new tab",
                    shiny::icon("external-link-alt")
                  )
                )
              )
            ),
            bslib::card_body(
              shiny::div(
                id = "iframe-container",
                class = "iframe-wrap",
                shiny::p(
                  id = "iframe-placeholder",
                  "Loading documentation..."
                ),
                shiny::tags$iframe(
                  id = "content-iframe",
                  name = "content-iframe",
                  src = "https://quarto.org",
                  style = "width:100%; height:100%; border:0;"
                )
              )
            )
          )
        )
      ),
      shiny::tags$script(
        shiny::HTML(
          r"---(
          window.IFR_HISTORY = [];
          window.IFR_INDEX = -1;
          window.shinychat_always_open_external_links = true;

          function updateNavButtons(){
            var back = document.getElementById('iframe-back');
            var fwd = document.getElementById('iframe-forward');
            if (back) back.disabled = !(window.IFR_INDEX > 0);
            if (fwd) fwd.disabled = !(window.IFR_INDEX >= 0 && window.IFR_INDEX < window.IFR_HISTORY.length - 1);
          }

          function pushHistory(url){
            if (!url) return;
            if (window.IFR_INDEX < window.IFR_HISTORY.length - 1) {
              window.IFR_HISTORY = window.IFR_HISTORY.slice(0, window.IFR_INDEX + 1);
            }
            window.IFR_HISTORY.push(url);
            window.IFR_INDEX = window.IFR_HISTORY.length - 1;
            updateNavButtons();
          }

          function goTo(url, push){
            var iframe = document.getElementById('content-iframe');
            if (!iframe || !url) return;
            if (push) pushHistory(url);
            iframe.src = url;
          }

          function ensureIframeShown(){
            var iframe = document.getElementById('content-iframe');
            if (!iframe) return;
            try { iframe.removeEventListener('load', iframe._onload || function(){}); } catch(e) {}
            iframe._onload = function(){
              var ph = document.getElementById('iframe-placeholder');
              if (ph) ph.style.display = 'none';
              iframe.style.display = 'block';
              if (window.IFR_HISTORY.length === 0 && iframe.src) {
                pushHistory(iframe.src);
              }
              updateNavButtons();
            };
            iframe.addEventListener('load', iframe._onload);
          }
          ensureIframeShown();

          function openInNewTab(url) { try { window.open(url, '_blank', 'noopener'); } catch (e) {} }

          document.addEventListener('click', function(ev){
            var btn = ev.target.closest('#open-preview-external');
            if (!btn) return;
            var iframe = document.getElementById('content-iframe');
            var url = (window.IFR_INDEX >= 0) ? window.IFR_HISTORY[window.IFR_INDEX] : (iframe && iframe.src);
            if (url) openInNewTab(url);
          }, true);

          document.addEventListener('click', function(ev){
            var root = document.querySelector('.content-split');
            if (!root) return;
            var btn = ev.target.closest('#toggle-chat');
            if (btn) {
              root.classList.add('collapsed-left');
              ev.preventDefault();
              return false;
            }
            var reveal = ev.target.closest('#toggle-chat-show');
            if (reveal) {
              root.classList.remove('collapsed-left');
              ev.preventDefault();
              return false;
            }
          }, true);

          (function(){
            var res = document.getElementById('split-resizer');
            var root = document.querySelector('.content-split');
            if (!res || !root) return;
            var dragging = false, startX = 0, startWidth = 0, bbox = null;
            var save = function(px){ try { localStorage.setItem('qh_split_width_px', String(px)); } catch(e){} };
            var load = function(){ try { return parseInt(localStorage.getItem('qh_split_width_px')||'',10); } catch(e){ return NaN; } };
            var apply = function(px){ if (!isFinite(px)) return; root.style.setProperty('--left-pane-width', px + 'px'); };
            var init = load(); if (isFinite(init) && init > 200) apply(init);

            res.addEventListener('mousedown', function(e){
              dragging = true; startX = e.clientX; bbox = root.getBoundingClientRect();
              var left = root.querySelector('.left-pane'); startWidth = left ? left.getBoundingClientRect().width : 0;
              document.body.style.userSelect = 'none';
              root.classList.add('resizing');
              e.preventDefault();
              e.stopPropagation();
            }, true);
            window.addEventListener('mousemove', function(e){
              if (!dragging) return;
              var dx = e.clientX - startX; var newPx = Math.max(200, Math.min(bbox.width * 0.8, startWidth + dx));
              apply(newPx);
              e.preventDefault();
            }, true);
            window.addEventListener('mouseup', function(e){
              if (!dragging) return; dragging = false;
              var left = root.querySelector('.left-pane'); var w = left ? left.getBoundingClientRect().width : NaN;
              if (isFinite(w)) save(Math.round(w));
              document.body.style.userSelect = '';
              root.classList.remove('resizing');
            }, true);
          })();

          document.addEventListener('click', function(ev){
            var back = ev.target.closest('#iframe-back');
            if (back) {
              if (window.IFR_INDEX > 0) {
                window.IFR_INDEX -= 1;
                updateNavButtons();
                var url = window.IFR_HISTORY[window.IFR_INDEX];
                var iframe = document.getElementById('content-iframe');
                if (iframe) iframe.src = url;
              }
              ev.preventDefault(); return false;
            }
            var fwd = ev.target.closest('#iframe-forward');
            if (fwd) {
              if (window.IFR_INDEX >= 0 && window.IFR_INDEX < window.IFR_HISTORY.length - 1) {
                window.IFR_INDEX += 1;
                updateNavButtons();
                var url2 = window.IFR_HISTORY[window.IFR_INDEX];
                var iframe2 = document.getElementById('content-iframe');
                if (iframe2) iframe2.src = url2;
              }
              ev.preventDefault(); return false;
            }
          }, true);

          document.addEventListener('click', function(ev){
            var a = ev.target && ev.target.closest ? ev.target.closest('#chat a, #chat-pane a') : null;
            if (!a) return;
            var href = a.getAttribute('href');
            if (!href) return;
            if (!/^https?:\\/\\//i.test(href)) return;
            if (ev.ctrlKey || ev.metaKey) return;
            ev.preventDefault();
            ev.stopPropagation();
            if (ev.stopImmediatePropagation) ev.stopImmediatePropagation();
            ensureIframeShown();
            goTo(href, true);
            return false;
          }, true);

          document.addEventListener('auxclick', function(ev){
            var a = ev.target && ev.target.closest ? ev.target.closest('#chat a, #chat-pane a') : null;
            if (!a) return;
            if (ev.button !== 1) return;
            var href = a.getAttribute('href');
            if (!href) return;
            if (!/^https?:\\/\\//i.test(href)) return;
            ev.preventDefault();
            ev.stopPropagation();
            if (ev.stopImmediatePropagation) ev.stopImmediatePropagation();
            openInNewTab(href);
            return false;
          }, true);

          function findSidebar(){
            return document.querySelector('[data-bslib-sidebar], .bslib-page .sidebar, .sidebar');
          }
          function findMain(container){
            return (container && container.querySelector('.main, [data-bslib-main]')) || document.querySelector('.main, [data-bslib-main]');
          }
          function collapseHistory(){
            var sb = findSidebar();
            if (!sb) { document.body.classList.add('history-collapsed'); return; }
            var container = sb.parentElement;
            var main = findMain(container);
            if (!container.dataset.origGtc) container.dataset.origGtc = container.style.gridTemplateColumns || '';
            if (!sb.dataset.origStyle) sb.dataset.origStyle = sb.getAttribute('style') || '';
            if (main && !main.dataset.origFlex) {
              main.dataset.origFlex = main.style.flex || '';
              main.dataset.origWidth = main.style.width || '';
            }
            if (container) container.style.gridTemplateColumns = '0 1fr';
            sb.style.display = 'none';
            sb.style.width = '0';
            sb.style.minWidth = '0';
            sb.style.maxWidth = '0';
            if (main) { main.style.flex = '1 1 auto'; main.style.width = '100%'; }
            document.body.classList.add('history-collapsed');
          }
          function expandHistory(){
            var sb = findSidebar();
            var container = sb ? sb.parentElement : null;
            var main = findMain(container);
            if (container && typeof container.dataset.origGtc !== 'undefined') {
              container.style.gridTemplateColumns = container.dataset.origGtc;
            }
            if (sb && typeof sb.dataset.origStyle !== 'undefined') {
              sb.setAttribute('style', sb.dataset.origStyle);
            } else if (sb) {
              sb.removeAttribute('style');
            }
            if (main) {
              if (typeof main.dataset.origFlex !== 'undefined') main.style.flex = main.dataset.origFlex;
              if (typeof main.dataset.origWidth !== 'undefined') main.style.width = main.dataset.origWidth;
            }
            document.body.classList.remove('history-collapsed');
          }
          if (document.body.classList.contains('history-collapsed')) collapseHistory();
          document.addEventListener('click', function(ev){ return true; }, true);
          )---"
        )
      )
    )
  )
}

#' Internal server for the Quarto Help app
#' @noRd
quartohelp_app_server <- function(
  initial_chat = NULL,
  chat_factory = configure_chat,
  initial_question = NULL
) {
  force(chat_factory)

  function(input, output, session) {
    first <- TRUE
    initial <- initial_chat
    pending_question <- shiny::reactiveVal(initial_question)

    make_chat <- function() {
      if (first) {
        first <<- FALSE
        if (!is.null(initial)) {
          chat_obj <- initial
          initial <<- NULL
          return(chat_obj)
        }
      }
      chat_factory()
    }

    chat <- shiny::reactiveVal(make_chat())
    chat_gen <- shiny::reactiveVal(1L)

    output$chat_panel <- shiny::renderUI({
      shinychat::chat_mod_ui(paste0("chat_", chat_gen()), height = "100%")
    })

    shiny::observeEvent(chat_gen(), {
      module_id <- paste0("chat_", chat_gen())
      module <- shinychat::chat_mod_server(module_id, chat())

      question <- pending_question()
      if (!is.null(question)) {
        pending_question(NULL)
        session$onFlushed(function() {
          module$update_user_input(value = question, submit = TRUE)
        }, once = TRUE)
      }
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$clear_chat, {
      chat(make_chat())
      chat_gen(shiny::isolate(chat_gen()) + 1L)
    })
  }
}
