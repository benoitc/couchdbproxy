function(doc) {
    if (doc.type == "node" && doc.active == true) {
         emit(doc.nodename, [doc.machine, doc.port]);
    }
}