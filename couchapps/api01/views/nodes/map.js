function(doc) {
  if (doc.type == "node" && doc.ips) 
    emit(doc._id, null)
}