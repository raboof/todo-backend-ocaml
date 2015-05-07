while true; do
  inotifywait -e attrib *.ml
  make publish
done
