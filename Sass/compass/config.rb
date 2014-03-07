# Require any additional compass plugins here.
# -----------------------------------------------------------------------------
project_path = File.expand_path("..",File.dirname(__FILE__))


http_path = "/"
css_dir = "../Static/css"
sass_dir = "assets/css"
images_dir = "../Static/img"
javascripts_dir = "../Static/js"
#svg_dir = "assets/svg"
#fonts_dir = "assets/fonts"

# Output style and comments
# -----------------------------------------------------------------------------

# You can select your preferred output style here (can be overridden via the command line):
# output_style = :expanded or :nested or :compact or :compressed
# Over-ride with force compile to change output style with: compass compile --output-style compressed --force
output_style = :compressed

line_comments = false
cache = true
color_output = false # required for Mixture

require 'sass-globbing'


# SASS core
# -----------------------------------------------------------------------------

# Chrome needs a precision of 7 to round properly
Sass::Script::Number.precision = 7
