## The MIT License (MIT)
##
## Copyright (c) 2014-2024
## Savin Max <mafei.198@gmail.com>
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.


task :setup => :environment do
  puts ">>>>>>>>> Create Database"
  Rake::Task['db:create'].execute
  puts ""

  puts ">>>>>>>>> Migrate Database"
  Rake::Task['db:migrate'].execute
  puts ""

  puts ">>>>>>>>> Generate Configs"
  Rake::Task['generate_config'].execute
  puts ""

  puts ">>>>>>>>> Generate Record"
  Rake::Task['generate_record'].execute
  puts ""

  puts ">>>>>>>>> Generate Database Config Record"
  Rake::Task['generate_database_record'].execute
  puts ""

  puts ">>>>>>>>> Generate Wordfilter"
  Rake::Task['generate_wordfilter'].execute
  puts ""

  puts ">>>>>>>>> Generate API"
  Rake::Task['generate_api'].execute
  puts ""
end