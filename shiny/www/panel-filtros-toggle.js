document.addEventListener('DOMContentLoaded', function() {
  setTimeout(function() {
    Shiny.addCustomMessageHandler('panel_filtros_toggle', function(_) {
      var p = document.getElementById('panel_filtros');
      if (!p) return;
      p.classList.toggle('ec-collapsed');
      var btn = document.querySelector('#toggle_filtros');
      if (btn) {
        var icon = btn.querySelector('i');
        if (p.classList.contains('ec-collapsed')) {
          btn.title = 'Expandir';
          if (icon) icon.className = 'fa fa-angle-double-right';
        } else {
          btn.title = 'Minimizar';
          if (icon) icon.className = 'fa fa-angle-double-left';
        }
      }
    });
  }, 100);
});
