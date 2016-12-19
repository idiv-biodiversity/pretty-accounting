git.baseVersion := "0.1.0"

git.gitTagToVersionNumber := { tag: String =>
  if (tag matches "[0-9]+\\..*") Some(tag)
  else None
}
