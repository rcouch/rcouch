# Ruby task file
#
# Run with
#
#   rake copy_couch_db_binary

desc "Recursively copy build files from ~/src/CouchDb into CouchDBX for inclusion in app"
task :copy_couch_db_binary do
  source_dir = File.expand_path("~/src/CouchDb")
  raise "The source directory is missing: '#{source_dir}'" unless File.exists?(source_dir)
  Dir["#{source_dir}/**/*"].each do |filename|
    relative_dest_path = "CouchDb/" + filename.gsub(source_dir, "")
    FileUtils.mkdir_p(File.dirname(relative_dest_path))
    if File.file?(filename)
      FileUtils.cp(filename, relative_dest_path)
    end
  end
end
