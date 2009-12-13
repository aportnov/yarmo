require 'rake'
require 'rake/clean'

CLEAN.include(['ebin/*.beam', 'ebin-test/*.beam' '*.dump'])

namespace :erlang do
  
  def for_deps(&block)
    root = "./deps"
    Dir.foreach("./deps")do |f| 
      yield root, f unless f =~ /^[.].*/
    end
  end
  
  desc 'compile all source files'
  task :compile => [:clean] do
    for_deps do |dir, f|
      sh "cd #{dir}/#{f} && make"
    end  
    
    sh "make"
    FileList['test/**/*.erl'].each{|f| sh "erlc -pa ebin -o ebin-test #{f}" }
  end

  desc "run tests"
  task :test => [:compile] do
    deps = []
    for_deps do |dir, f|
      deps << "-pa #{dir}/#{f}/ebin"
    end  
    
    FileList['test/**/*_test.erl'].each do |tf| 
      sh "erl -noshell -pa ebin -pa ebin-test #{deps.join(' ')} -s #{File.basename(tf, '.erl')} test -s init stop"
    end  
  end
  
end

task :default => ['erlang:compile']