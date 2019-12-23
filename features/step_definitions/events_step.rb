require 'json'
require 'rest-client'
require 'timeout'
require 'tmpdir'

$response = nil
$variables = {}

Given(/"([^"]+)" is a declared subject kind with \[(.+)\] actions/) do |kind, actions|
  $response = RestClient.post "http://localhost:8080/kinds/" + kind, JSON.parse("[#{actions}]").to_json, {content_type: :json, accept: :json}
  expect($response.code).to eq(200), $response.body
end

When(/declaring a new "([^"]+)" subject as "([^"]+)" with data (.+)/) do |kind, uid, data|
  $response = RestClient.post "http://localhost:8080/kinds/" + kind + "/subjects/" + uid, JSON.parse(data).to_json, {content_type: :json, accept: :json}
  expect($response.code).to eq(200), $response.body
end

Then(/an event uid \$(.+) should be retrieved/) do |event_uid|
  $variables[event_uid] = JSON.parse($response.body)['event_uid']
  expect($variables[event_uid]).not_to be_empty, $response.body
end

When(/declaring a new "([^"]+)" event of "([^"]+)" with "([^"]+)" action and (.+) data/) do |kind, uid, action, data|
  $response = RestClient.post "http://localhost:8080/kinds/" + kind + "/subjects/" + uid + "/events/" + action, JSON.parse(data).to_json, {content_type: :json, accept: :json}
  expect($response.code).to eq(200), $response.body
end

When(/fetching "([^"]+)" events of "([^"]+)"/) do |kind, uid|
  $response = RestClient.get "http://localhost:8080/kinds/" + kind + "/subjects/" + uid + "/events/", {accept: :json}
  expect($response.code).to eq(200)
end

Then(/the following events are retrieved:/) do |table|
  raw = table.raw
  headers = raw.shift
  expected = raw.map do |line|
    intr_line = line.map do |l|
      if l.start_with?('$')
        s = $variables[l[1..-1]]
        expect(l).not_to be_empty
        s
      elsif "[{\"'".include?(l[0])
        JSON.parse(l)
      else
        l
      end
    end
    Hash[headers.zip(intr_line)]
  end
  expect(expected).to eq(JSON.parse($response.body)), $response.body
end

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
Before do

  $server = fork do
    Dir.mktmpdir do |dir|
      exec 'stack', 'exec', 'hermes', dir + '/hermes.db'
    end
  end

  Timeout::timeout(5) do
    sleep(0.1) until port_open?("localhost", 8080)
  end
end

After do
  Process.kill "TERM", $server
end
