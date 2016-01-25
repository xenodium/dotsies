# Converts Xcode's code snippets into Emacs yasnippets

import plistlib
import re
import os
import sys

from collections import namedtuple


def load_raw_xcode_snippets_plist():
  return plistlib.readPlist(
      "/Applications/Xcode.app/Contents/Frameworks/IDEKit.framework/Versions/A/Resources/SystemCodeSnippets.codesnippets")


def parse_raw_xcode_yasnippet(xcode_snippet):
  XcodeSnippet = namedtuple('XcodeSnippet',
                            'prefix title summary language content')
  return XcodeSnippet(xcode_snippet["IDECodeSnippetCompletionPrefix"],
                      xcode_snippet["IDECodeSnippetTitle"],
                      xcode_snippet["IDECodeSnippetSummary"],
                      xcode_snippet["IDECodeSnippetLanguage"],
                      xcode_snippet["IDECodeSnippetContents"])


def convert_xcode_content_to_yasnippet(content):
  pattern = re.compile(r'<#(.*?)#>', re.IGNORECASE)
  argument_number = [0]

  def replace_function(match):
    argument_number[0] += 1
    return "${%d:%s}" % (argument_number[0], match.groups()[0])

  return pattern.sub(replace_function, content)


def convert_xcode_to_yasnippet(xcode_snippet):
  yasnippet = ""
  yasnippet += "#name : %s\n" % xcode_snippet.title
  yasnippet += "#key : %s\n" % xcode_snippet.prefix
  yasnippet += "# --\n"
  yasnippet += convert_xcode_content_to_yasnippet(xcode_snippet.content)
  return yasnippet


def convert_to_yasnippets():
  raw_xcode_snippets = load_raw_xcode_snippets_plist()
  for raw_xcode_snippet in raw_xcode_snippets:
    xcode_snippet = parse_raw_xcode_yasnippet(raw_xcode_snippet)
    if not xcode_snippet.language or not xcode_snippet.prefix:
      continue
    language_to_emacs_mode = {
        "Xcode.SourceCodeLanguage.C": "c-mode",
        "Xcode.SourceCodeLanguage.C-Plus-Plus": "c++-mode",
        "Xcode.SourceCodeLanguage.Objective-C": "objc-mode"
    }
    if xcode_snippet.language not in language_to_emacs_mode.keys():
      continue
    out_dir = language_to_emacs_mode[xcode_snippet.language]
    if not os.path.exists(out_dir):
      os.mkdir(out_dir)
    yasnippet_path = os.path.join(out_dir, "%s.yasnippet" % xcode_snippet.prefix)
    with open(yasnippet_path, 'w') as f:
      yasnippet = convert_xcode_to_yasnippet(xcode_snippet)
      f.write(yasnippet)


def main():
  if sys.platform != "darwin":
    print "Error: Not on Mac OS X"
    return
  convert_to_yasnippets()


if __name__ == "__main__":
  main()
