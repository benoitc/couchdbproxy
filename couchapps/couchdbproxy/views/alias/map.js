function(doc) { 
if (doc.type == "alias") {
key = doc.hostname.split(".");
emit(key, [doc.nodename,doc.port,doc.path]); 
}
}