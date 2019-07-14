require 'timeout'
require 'json'
require 'rest-client'

$response = nil
$variables = {}

Given(/"([^"]+)" is a declared subject kind with \[(.+)\] actions/) do |kind, actions|
  $response = RestClient.post "http://localhost:8080/kinds/" + kind, JSON.parse("[#{actions}]").to_json, {content_type: :json, accept: :json}
  expect($response.code).to eq(200)
end

When(/declaring a new "([^"]+)" subject as "([^"]+)" with data (.+)/) do |kind, uid, data|
  $response = RestClient.post "http://localhost:8080/kinds/" + kind + "/subjects/" + uid, JSON.parse(data).to_json, {content_type: :json, accept: :json}
  expect($response.code).to eq(200)
end

Then(/an event uid \$(.+) should be retrieved/) do |event_uid|
  $variables[event_uid] = JSON.parse($response.body)['event_uid']
  expect($variables[event_uid]).not_to be_empty
end

When(/declaring a new "([^"]+)" event of "([^"]+)" with "([^"]+)" action and (.+) data/) do |kind, uid, action, data|
  $response = RestClient.post "http://localhost:8080/kinds/" + kind + "/subjects/" + uid + "/events/" + action, JSON.parse(data).to_json, {content_type: :json, accept: :json}
  expect($response.code).to eq(200)
end

When(/fetching "([^"]+)" events of "([^"]+)"/) do |kind, uid|
  $response = RestClient.get "http://localhost:8080/kinds/" + kind + "/subjects/" + uid + "/events/", {accept: :json}
  expect($response.code).to eq(200)
end

Then(/the following events are retrieved:/) do |table|
  raw = table.raw
  headers = raw.shift
  expected = raw.map(&-> (line) { Hash[headers.zip(line)] })
  expect(expected).to eq($response.body)
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
    exec 'stack', 'exec', 'hermes-memory'
  end

  Timeout::timeout(5) do
    sleep(0.01) until port_open?("localhost", 8080)
  end
end

After do
  Process.kill "TERM", $server
end
