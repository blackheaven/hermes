def port_open?(ip, port, seconds=1)
  Timeout::timeout(seconds) do
    begin
      TCPSocket.new(ip, port).close
      true
    rescue Errno::ECONNREFUSED, Errno::EHOSTUNREACH
      false
    end
  end
rescue Timeout::Error
  false
end

$server = nil
$rabbitmq_container = nil
$rabbitmq_container_started = false
$rabbitmq_container_ip = nil

Around do |scenario, block|
  if $server
    Process.kill "TERM", $server
  end

  if $rabbitmq_container.nil?
    Timeout::timeout(45) do
      Open3.popen3("docker run --detach --rm --hostname rabbitmq rabbitmq:3.8.3-management") do |stdin, stdout, stderr, thread|
         $rabbitmq_container = stdout.read.chomp
      end

      while !$rabbitmq_container_started
        Open3.popen3("docker logs #{$rabbitmq_container}") do |stdin, stdout, stderr, thread|
           stdout.each_line do |l|
             $rabbitmq_container_started = true if l.start_with?(' completed with ')
           end
        end
        sleep(0.2)
      end

      Open3.popen3("docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' #{$rabbitmq_container}") do |stdin, stdout, stderr, thread|
        $rabbitmq_container_ip = stdout.read.chomp
      end
    end
  end

  $server = fork do
    Dir.mktmpdir do |dir|
      exec 'stack', 'exec', 'hermes', dir + '/hermes.db', "amqp://guest:guest@#{$rabbitmq_container_ip}:5672/%2F"
    end
  end

  Timeout::timeout(5) do
    sleep(0.1) until port_open?("localhost", 8080)
  end

  block.call
end

After do
  Process.kill "TERM", $server
end

at_exit do
  mutex = Mutex.new
  Thread.new do
    mutex.synchronize {
      `docker rm -f #{$rabbitmq_container}`
    }
  end
  sleep(2)
  mutex.synchronize {
    $rabbitmq_container = nil
    $rabbitmq_container_started = false
    $rabbitmq_container_ip = nil
  }
end

