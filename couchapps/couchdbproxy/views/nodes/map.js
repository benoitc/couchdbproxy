function(doc) {
    if (doc.type == "node") {
       emit(doc.nodename, doc);
    }
}