CosineSimilarity <-
function(va, vb) {
  # Computer cosine similarity between two numeric vectors of the same length

  crossprod(va, vb) / sqrt(crossprod(va) * crossprod(vb))
}
