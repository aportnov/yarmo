require 'rake'
require 'rake/clean'

CLEAN.include(['ebin/*.beam', 'ebin-test/*.beam' '*.dump'])

namespace :erlang do
  
  desc 'compile all source files'
  task :compile => [:clean] do
    root = "#{File.dirname(File.expand_path(__FILE__))}/deps"
    
    Dir.foreach("./deps")do |f| 
      sh "cd #{root}/#{f} && make" unless f =~ /^[.].*/
    end
    
    sh "make"
    FileList['test/**/*.erl'].each{|f| sh "erlc -pa ebin -o ebin-test #{f}" }
  end

  desc "run tests"
  task :test => [:compile] do
    FileList['test/**/*_test.erl'].each do |tf| 
      sh "erl -noshell -pa ebin -pa ebin-test -s #{File.basename(tf, '.erl')} test -s init stop"
    end  
  end
  
end

task :default => ['erlang:compile']