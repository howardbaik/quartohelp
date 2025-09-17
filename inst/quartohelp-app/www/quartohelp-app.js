(() => {
  const init = () => {
    if (!document.body) {
      window.requestAnimationFrame(init);
      return;
    }

    window.IFR_HISTORY = [];
    window.IFR_INDEX = -1;
    window.shinychat_always_open_external_links = true;

    const qhLog = (...args) => {
      if (typeof console !== 'undefined' && console.debug) {
        console.debug('[quartohelp]', ...args);
      }
    };

    qhLog('inline script initialised');

    const HTTP_RE = /^https?:\/\//i;

    function updateNavButtons(){
      const back = document.getElementById('iframe-back');
      const fwd = document.getElementById('iframe-forward');
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
      const iframe = document.getElementById('content-iframe');
      if (!iframe || !url) return;
      if (push) pushHistory(url);
      iframe.src = url;
    }

    function ensureIframeShown(){
      const iframe = document.getElementById('content-iframe');
      if (!iframe) return;
      try {
        iframe.removeEventListener('load', iframe._onload || (() => {}));
      } catch(e) {}
      iframe._onload = () => {
        const ph = document.getElementById('iframe-placeholder');
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

    function openInNewTab(url) {
      try {
        window.open(url, '_blank', 'noopener');
      } catch (e) {}
    }

    document.addEventListener('click', (ev) => {
      const btn = ev.target.closest('#open-preview-external');
      if (!btn) return;
      const iframe = document.getElementById('content-iframe');
      const url = (window.IFR_INDEX >= 0)
        ? window.IFR_HISTORY[window.IFR_INDEX]
        : (iframe && iframe.src);
      if (url) openInNewTab(url);
    }, true);

    document.addEventListener('click', (ev) => {
      const root = document.querySelector('.content-split');
      if (!root) return;
      const btn = ev.target.closest('#toggle-chat');
      if (btn) {
        qhLog('collapse chat click');
        root.classList.add('collapsed-left');
        ev.preventDefault();
        return false;
      }
      const reveal = ev.target.closest('#toggle-chat-show');
      if (reveal) {
        qhLog('expand chat click');
        root.classList.remove('collapsed-left');
        ev.preventDefault();
        return false;
      }
    }, true);

    (function(){
      function setupSplit(){
        const res = document.getElementById('split-resizer');
        const root = document.querySelector('.content-split');
      if (!res || !root) return;
      const left = root.querySelector('.left-pane');
      if (!left) return;

      const save = (px) => {
        try { localStorage.setItem('qh_split_width_px', String(px)); } catch(e){}
      };
      const load = () => {
        try { return parseInt(localStorage.getItem('qh_split_width_px') || '', 10); }
        catch(e) { return NaN; }
      };
      const apply = (px) => {
        if (!Number.isFinite(px)) return;
        root.style.setProperty('--left-pane-width', `${px}px`);
      };

      const init = load();
      if (Number.isFinite(init) && init > 200) {
        apply(init);
        left.style.flex = `0 0 ${init}px`;
        left.style.width = `${init}px`;
      }

      const startDrag = (startX) => {
        const rootBox = root.getBoundingClientRect();
        const startWidth = left.getBoundingClientRect().width;
        const maxWidth = rootBox.width * 0.8;

        const update = (px, persist) => {
          const clamped = Math.max(200, Math.min(maxWidth, px));
          apply(clamped);
          left.style.flex = `0 0 ${clamped}px`;
          left.style.width = `${clamped}px`;
          if (persist) save(Math.round(clamped));
          qhLog('resizer update', { px: clamped, persist: !!persist });
        };

        const onMove = (clientX) => {
          if (!Number.isFinite(clientX)) return;
          update(startWidth + (clientX - startX), false);
        };

        const finish = (clientX) => {
          document.body.style.userSelect = '';
          root.classList.remove('resizing');
          window.removeEventListener('mousemove', mouseMove, false);
          window.removeEventListener('mouseup', mouseUp, false);
          window.removeEventListener('touchmove', touchMove, false);
          window.removeEventListener('touchend', touchEnd, false);
          window.removeEventListener('touchcancel', touchEnd, false);
          if (Number.isFinite(clientX)) {
            update(startWidth + (clientX - startX), true);
          }
        };

        const mouseMove = (e) => {
          onMove(e.clientX);
          e.preventDefault();
        };
        const mouseUp = (e) => {
          finish(e.clientX);
          e.preventDefault();
        };
        const touchMove = (e) => {
          if (!e.touches || !e.touches.length) return;
          onMove(e.touches[0].clientX);
          e.preventDefault();
        };
        const touchEnd = (e) => {
          const clientX = (e.changedTouches && e.changedTouches.length)
            ? e.changedTouches[0].clientX
            : NaN;
          finish(clientX);
          e.preventDefault();
        };

        document.body.style.userSelect = 'none';
        root.classList.add('resizing');
        window.addEventListener('mousemove', mouseMove, { passive: false });
        window.addEventListener('mouseup', mouseUp, { passive: false });
        window.addEventListener('touchmove', touchMove, { passive: false });
        window.addEventListener('touchend', touchEnd, { passive: false });
        window.addEventListener('touchcancel', touchEnd, { passive: false });
      };

      res.addEventListener('mousedown', (e) => {
        if (e.button !== 0) return;
        qhLog('drag start (mouse)', { x: e.clientX });
        startDrag(e.clientX);
        e.preventDefault();
        e.stopPropagation();
      }, true);

      res.addEventListener('touchstart', (e) => {
        if (!e.touches || !e.touches.length) return;
        qhLog('drag start (touch)', { x: e.touches[0].clientX });
        startDrag(e.touches[0].clientX);
        e.preventDefault();
        e.stopPropagation();
      }, { capture: true, passive: false });
    }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', setupSplit, { once: true });
      } else {
        setupSplit();
      }
    })();

    document.addEventListener('click', (ev) => {
      const back = ev.target.closest('#iframe-back');
      if (back) {
        if (window.IFR_INDEX > 0) {
          window.IFR_INDEX -= 1;
          updateNavButtons();
          const url = window.IFR_HISTORY[window.IFR_INDEX];
          const iframe = document.getElementById('content-iframe');
          if (iframe) iframe.src = url;
        }
        ev.preventDefault();
        return false;
      }
      const fwd = ev.target.closest('#iframe-forward');
      if (fwd) {
        if (window.IFR_INDEX >= 0 && window.IFR_INDEX < window.IFR_HISTORY.length - 1) {
          window.IFR_INDEX += 1;
          updateNavButtons();
          const url2 = window.IFR_HISTORY[window.IFR_INDEX];
          const iframe2 = document.getElementById('content-iframe');
          if (iframe2) iframe2.src = url2;
        }
        ev.preventDefault();
        return false;
      }
    }, true);

    document.addEventListener('click', (ev) => {
      const a = ev.target && ev.target.closest ? ev.target.closest('#chat a, #chat-pane a') : null;
      if (!a) return;
      const href = a.getAttribute('href');
      if (!href) return;
      if (!HTTP_RE.test(href)) return;
      if (ev.ctrlKey || ev.metaKey) return;
      qhLog('chat link click intercepted', { href });
      ev.preventDefault();
      ev.stopPropagation();
      if (ev.stopImmediatePropagation) ev.stopImmediatePropagation();
      ensureIframeShown();
      goTo(href, true);
      return false;
    }, true);

    document.addEventListener('auxclick', (ev) => {
      const a = ev.target && ev.target.closest ? ev.target.closest('#chat a, #chat-pane a') : null;
      if (!a) return;
      if (ev.button !== 1) return;
      const href = a.getAttribute('href');
      if (!href) return;
      if (!HTTP_RE.test(href)) return;
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
      const sb = findSidebar();
      if (!sb) {
        document.body.classList.add('history-collapsed');
        return;
      }
      const container = sb.parentElement;
      const main = findMain(container);
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
      if (main) {
        main.style.flex = '1 1 auto';
        main.style.width = '100%';
      }
      document.body.classList.add('history-collapsed');
    }
    function expandHistory(){
      const sb = findSidebar();
      const container = sb ? sb.parentElement : null;
      const main = findMain(container);
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
    if (document.body && document.body.classList.contains('history-collapsed')) collapseHistory();
    document.addEventListener('click', function(ev){ return true; }, true);

    window.addEventListener('error', (event) => {
      const target = event?.target;
      const candidate = (target && (target.src || target.href)) || '';
      if (typeof candidate === 'string' && candidate.endsWith('.map')) {
        event.preventDefault();
        event.stopImmediatePropagation();
      }
    }, true);
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init, { once: true });
  } else {
    init();
  }
})();
