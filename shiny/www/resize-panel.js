document.addEventListener('DOMContentLoaded', function() {
  setTimeout(function() {
    var panel = document.getElementById('panel_datos');
    var handle = document.querySelector('.ec-panel-resize-handle');
    if (!panel || !handle) return;
    var minH = 220, maxH = Math.floor(window.innerHeight * 0.85);
    var resizing = false, startY = 0, startH = 0;
    handle.addEventListener('mousedown', function(e) {
      e.preventDefault();
      e.stopPropagation();
      var wrapper = panel.closest('.ec-panel-datos');
      if (wrapper.classList.contains('ec-panel-closed') || wrapper.classList.contains('ec-panel-minimized')) return;
      resizing = true;
      startY = e.clientY;
      startH = panel.offsetHeight;
      document.body.style.cursor = 'ns-resize';
      document.body.style.userSelect = 'none';
    });
    document.addEventListener('mousemove', function(e) {
      if (!resizing) return;
      var dy = startY - e.clientY;
      var h = Math.max(minH, Math.min(maxH, startH + dy));
      panel.style.height = h + 'px';
    });
    document.addEventListener('mouseup', function() {
      if (resizing) {
        resizing = false;
        document.body.style.cursor = '';
        document.body.style.userSelect = '';
      }
    });
  }, 500);
});

