#!/usr/bin/env watchr

begin; require 'watchr/event_handlers/em'; rescue LoadError; end
begin; require 'watchr/deps'; rescue LoadError; end

# p "reload"

watch( %r(.*), :modified, lambda { |md| File.directory? md[0] } ) do |md|
  File.directory? md[0] and raise Watchr::Refresh
end

watch( %r(dramatis.watchr), :modified ) do |md|
  raise Watchr::Refresh
end

watch( %r((^(public|spec)/.*)\.haml$), [:load, :created, :modified] ) do |md, event|
  skip = false
  if event == :load
    if File.exist? "#{md[1]}.html"
      if File.mtime( "#{md[1]}.html" ) > File.mtime( "#{md[1]}.haml" )
        skip = true
      end
    end
  end

  if !skip
    # FIX: check mtime diffs on load
    # FIX: sign html with hash and don't overwrite/delete on bad hash
    # FIX: support deleted and remove html file
    
    cmd = "rm -f #{md[1]}.html && haml -f html5 #{md[0]} #{md[1]}.html && chmod 444 #{md[1]}.html"
    puts cmd
    system cmd
    if  $?.signaled? && $?.termsig == 2
      Process.kill 2, 0
    end
  end
end

watch( %r((^(public|spec)/.*)\.sass$), [:load, :created, :modified] ) do |md, event|
  skip = false
  if event == :load
    if File.exist? "#{md[1]}.css"
      if File.mtime( "#{md[1]}.css" ) > File.mtime( "#{md[1]}.sass" )
        skip = true
      end
    end
  end

  if !skip
    # FIX: check mtime diffs on load
    # FIX: sign css with hash and don't overwrite/delete on bad hash
    # FIX: support deleted and remove csss file
    
    cmd = "rm -f #{md[1]}.css && sass #{md[0]} #{md[1]}.css && chmod 444 #{md[1]}.css"
    puts cmd
    system cmd
    if  $?.signaled? && $?.termsig == 2
      Process.kill 2, 0
    end
  end
end

map_to_test = lambda do |file, event|
  case file
  when %r(spec/(.*)([Ss]pec)\.js$)
    # Run JS spec's using parallel HTML file if it exists
    prefix = $~[1];
    prefix.sub! %r(_$), ""
    files = Dir[prefix+".*htm*"]
    if html = files.detect { |f| f =~ %r(\.x?html?) }
      event == :load ? nil : html
    else 
      file
    end
  else; file
  end
end

jazrb = lambda do |*args|
  files = []
  # boy, clean this up, but call/splat are subtle
  if Array === args[0]
    args = args[0][0]
    files = args.map { |pair| map_to_test.call( pair[0][0], pair[1] ) }
    files.compact!
    files.uniq!
  else
    (file, event) = *args
    file = map_to_test.call file, event
    if file
      files = [ file ]
    end
  end
  if !files.empty?
    deps = ""
    begin deps = "--deps #{db_path}"; rescue; end
    cmd = "jazrb #{deps} #{files.join(" ")}"
    puts cmd
    system cmd
    puts "exit status: #{$?.exitstatus}" if $?.exited? && $?.exitstatus != 0
    if  $?.signaled? && $?.termsig == 2
      Process.kill 2, 0
    end
  end
end

# watch( %r((spec/.*[Ss]pec)\.(html|js)$), [ :load, :created, :modified ] ) do |md|

watch( %r(^(spec/.*[Ss]pec)\.js$), [ :load, :created, :modified ], nil, :batch => :js ) do |events|
  jazrb.call events
end

watch( %r(.*\.x?html?$), [ :load, :created, :modified ], nil, :batch => :html ) do |events|
  jazrb.call events
end

# don't watch vendor
watch( %r(^vendor/(jquery|raphael|underscore|strophejs|jazrb)), [ nil, :load ], lambda { false } )

Signal.trap('QUIT') do
  EM.stop
end

# Local Variables:
# mode:ruby
# End:
