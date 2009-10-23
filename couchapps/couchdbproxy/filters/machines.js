function(doc, req) {
  if (doc.type == "machine") {
    return [doc.name, doc.ips];
  }
}