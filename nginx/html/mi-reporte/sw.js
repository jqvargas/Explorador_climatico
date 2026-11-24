/* Legacy /mi-reporte/ SW: unregister so root /sw.js can take over. */
self.addEventListener("install", function () {
  self.skipWaiting();
});
self.addEventListener("activate", function (event) {
  event.waitUntil(
    self.registration.unregister().then(function () {
      return self.clients.matchAll({ type: "window" });
    }).then(function (clients) {
      clients.forEach(function (c) {
        if (c.url && typeof c.navigate === "function") c.navigate(c.url);
      });
    })
  );
});
