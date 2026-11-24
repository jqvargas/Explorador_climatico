document.addEventListener('DOMContentLoaded', function() {
  setTimeout(function() {
    var panel = document.getElementById('panel_datos');
    var handle = document.querySelector('.ec-panel-resize-handle');
    if (!panel || !handle) return;

    var mobileMql = window.matchMedia('(max-width: 767px)');
    function isMobilePanel() { return mobileMql.matches; }

    function maxPanelHeight() {
      var vh = window.visualViewport ? window.visualViewport.height : window.innerHeight;
      return Math.floor(vh * 0.85);
    }

    var minH = 220;
    var resizing = false;
    var startY = 0;
    var startH = 0;

    function beginResize(clientY) {
      var wrapper = panel.closest('.ec-panel-datos');
      if (!wrapper || wrapper.classList.contains('ec-panel-closed') || wrapper.classList.contains('ec-panel-minimized')) return;
      if (isMobilePanel()) return;
      resizing = true;
      startY = clientY;
      startH = panel.offsetHeight;
      document.body.style.cursor = 'ns-resize';
      document.body.style.userSelect = 'none';
    }

    function moveResize(clientY) {
      if (!resizing) return;
      var dy = startY - clientY;
      var h = Math.max(minH, Math.min(maxPanelHeight(), startH + dy));
      panel.style.height = h + 'px';
    }

    function endResize() {
      if (!resizing) return;
      resizing = false;
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
    }

    handle.addEventListener('mousedown', function(e) {
      e.preventDefault();
      e.stopPropagation();
      beginResize(e.clientY);
    });

    handle.addEventListener('touchstart', function(e) {
      if (isMobilePanel()) return;
      e.preventDefault();
      e.stopPropagation();
      if (e.touches && e.touches[0]) beginResize(e.touches[0].clientY);
    }, { passive: false });

    document.addEventListener('mousemove', function(e) {
      moveResize(e.clientY);
    });

    document.addEventListener('mouseup', endResize);

    document.addEventListener('touchmove', function(e) {
      if (!resizing || !e.touches || !e.touches[0]) return;
      e.preventDefault();
      moveResize(e.touches[0].clientY);
    }, { passive: false });

    document.addEventListener('touchend', endResize);
    document.addEventListener('touchcancel', endResize);
  }, 500);
});
