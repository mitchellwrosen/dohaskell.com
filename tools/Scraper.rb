require 'rubygems'
require 'mechanize'
require 'logger'
require 'pp'

prelude_url = "http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html"

mechanize = Mechanize.new
mechanize.log = Logger.new "mech.log"

page = mechanize.get prelude_url
functions = page/'.//p[@class="src"]'

BannedMonads = [
   "IO",
   "IOError"
];

def banned? text

   BannedMonads.each do |m|
      if text.include? m
         return true
      end
   end

   not text.include? " :: "
end

def get_types text
   text.slice!(/(.*) :: (.* => )?/)
   text.lstrip!

   types = []
   num_parens = 0
   type = ""
   ndx = 0

   while ndx < text.length do
      if text[ndx] == '('[0]
         num_parens += 1
      elsif text[ndx] == ')'[0]
         num_parens -= 1
      elsif num_parens == 0 and text[ndx, ' -> '.length] == ' -> '
         types << type
         type = ""
         ndx += ' -> '.length
         next
      end

      type << text[ndx]
      ndx += 1
   end

   types << type
end

functions.each do |function|
   text = function.inner_text().chomp "Source"

   if !(banned? text)

      name = /(.*) :: /.match(text).captures[0]
      constraints = /:: (.* => )/.match(text)

      types = get_types text

      puts "name: " + name
      if constraints
         puts "type constraints: " + constraints.captures[0]
      end
      print "type: "
      pp types
   end
end
