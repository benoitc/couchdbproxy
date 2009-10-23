function(doc) { 
  if (doc.type == "alias") {
    emit(doc.nodename, doc);
  }
}