#!/usr/bin/ruby
require 'rubygems'
require 'mechanize'
require 'logger'
require 'pp'

$mechanize = Mechanize.new
$mechanize.log = Logger.new "mech.log"

def escape_quotes text
   text.gsub( '"', '\"')
end

class Function
   @@functions = []

   def self.input_all form
      @@functions.each do |function|
         form['f2'] = function.to_s
         form.submit
      end
   end

   def self.puts_all
      @@functions.each do |function|
         puts function.to_s
      end
   end


   def self.append(modle, name, type_signature, num_args, doc)
      @@functions << Function.new(modle, name, type_signature, num_args, doc)
   end

   def initialize(modle, name, type_signature, num_args, doc)
      @module = modle
      @name = name
      @type_signature = type_signature
      @num_args = num_args
      @doc = doc
   end

   def to_s
      "LibFunction {libFunctionName = \"#{@name}\", libFunctionTypeSignature = \"#{@type_signature}\", " +
      "libFunctionNumArgs = #{@num_args}, libFunctionDocumentation = \"#{escape_quotes @doc}\", " +
      "libFunctionModule = \"#{@module}\"}"
   end
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

def processFunctionHtml functionHtml, moduleName
   text = functionHtml.inner_text().chomp "Source"

   if text.include? " :: "
      name = /(.*) :: /.match(text).captures[0]
      type_signature = /:: (.*)/.match(text).captures[0]

      num_args = (get_types! text).length

      doc = ""
      sibling = functionHtml.next_sibling
      if sibling and sibling['class'].include? "doc"
         sibling.xpath('.//a').each do |anchor|
            replacement = Nokogiri::XML::Node.new "div", anchor.parent
            replacement.inner_html = anchor.inner_html
            anchor.replace(replacement)
         end
         doc = sibling.inner_html
      end

      Function.append(moduleName, name, type_signature, num_args, doc)
   end
end

def readModules()
   Hash[*File.read('modules').split(/\s/)]
end

def scrapeModule moduleName, moduleUrl
   page = $mechanize.get moduleUrl
   functionsHtml = page/'.//p[@class="src"]'
   functionsHtml.each do |functionHtml|
      processFunctionHtml functionHtml, moduleName
   end
end

readModules().each do |moduleName, moduleUrl|
   $stderr.puts "Scraping #{moduleName} (#{moduleUrl})..."
   scrapeModule moduleName, moduleUrl
end

Function.puts_all
