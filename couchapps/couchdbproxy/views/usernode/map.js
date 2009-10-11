function(doc) {
    if (doc.type == "user" && doc.active == true) {
        emit(doc.username, [doc.nodename, doc.port]);
    }
}