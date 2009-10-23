function(doc) {
    if (doc.type == "node") {
       emit([doc.machine, doc.port], doc);
    }
}