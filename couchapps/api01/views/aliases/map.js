function(doc) {
  if (doc.type == "alias" && !doc.nodeame)
    emit(doc._id, null);
}