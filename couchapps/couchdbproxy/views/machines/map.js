function(doc) {
    if (doc.type == "machine") {
      emit(doc.name, doc.ips);
    }
}