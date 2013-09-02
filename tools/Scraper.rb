require 'rubygems'
require 'mechanize'
require 'logger'
require 'pp'

prelude_url = "http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html"

mechanize = Mechanize.new
mechanize.log = Logger.new "mech.log"

page = mechanize.get prelude_url
functions = page/'.//p[@class="src"]'

class Function
   @@functions = []

   def self.append(modle, constraints, name, userName, types, doc)
      @@functions << Function.new(modle, constraints, name, userName, types, doc)
   end

   def initialize(modle, constraints, name, userName, types, doc)
      @module = modle
      @constraints = constraints
      @name = name
      @userName = userName
      @types = types
      @doc = doc
   end
end

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

def get_types! text
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

      if !constraints
         constraints = ""
      end

      types = get_types! text

      doc = ""
      sibling = function.next_sibling
      if sibling and sibling['class'].include? "doc"
         sibling.xpath('.//a').each do |anchor|
            replacement = Nokogiri::XML::Node.new "div", anchor.parent
            replacement.inner_html = anchor.inner_html
            anchor.replace(replacement)
         end
         doc = sibling.inner_html
      end
      Function.append("Prelude", constraints, name, "my_" + name, types, doc)
   end
end
