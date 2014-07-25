import re
import sys


def _read_file(path):
  with open(path, 'r') as text_file:
    text = text_file.read()
  return text


def _write_file(text, path):
  with open(path, 'w') as text_file:
    text_file.write(text)


def _paste_text_into(from_text, to_text, name, delimiter):
  start_marker = '%s %s pasted - start' % (delimiter, name)
  end_marker = '%s %s pasted - end' % (delimiter, name)
  pattern = r'%s.*%s' % (start_marker, end_marker)

  match = re.search(pattern, to_text, flags=re.DOTALL)
  if not match:
    print 'appended install...'
    return '\n'.join([to_text,
                      start_marker,
                      from_text,
                      end_marker])

  print 'patched install...'
  to_text = re.sub(pattern, '', to_text, flags=re.DOTALL)
  return ''.join([to_text,
                  start_marker,
                  '\n\n',
                  from_text,
                  '\n\n',
                  end_marker])


def main():
  if len(sys.argv) < 5:
    sys.exit('usage: paste.py from/file.txt to/file.txt "name" "delimiter"')

  from_file_path = sys.argv[1]
  to_file_path = sys.argv[2]
  name = sys.argv[3]
  delimiter = sys.argv[4]

  from_file_text = _read_file(from_file_path)
  to_file_text = _read_file(to_file_path)

  new_text = _paste_text_into(from_file_text, to_file_text, name, delimiter)

  _write_file(new_text, to_file_path)

  print 'updated %s' % to_file_path

if __name__ == '__main__':
  main()
