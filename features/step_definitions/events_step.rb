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
  zip_table(table)
end

When(/^subscribing to the "([^"]+)" subject "([^"]+)" as "([^"]+)"/) do |kind, subject, subscriber|
  $response = RestClient.post "http://localhost:8080/subscribers/" + subscriber, {kind: kind, subject: subject}.to_json, {content_type: :json, accept: :json}
  expect($response.code).to eq(200)
end

When(/fetching subscriptions of "([^"]+)"/) do |subscriber|
  $response = RestClient.get "http://localhost:8080/subscribers/" + subscriber, {accept: :json}
  expect($response.code).to eq(200)
end

When(/^unsubscribing to the "([^"]+)" subject "([^"]+)" as "([^"]+)"/) do |kind, subject, subscriber|
  $response = RestClient.delete "http://localhost:8080/subscribers/" + subscriber, {kind: kind, subject: subject}.to_json, {content_type: :json, accept: :json}
  expect($response.code).to eq(200)
end

Then(/the following subscriptions are retrieved:/) do |table|
  zip_table(table)
end


def zip_table(table)
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
  structured_response = JSON.parse($response.body).map { |line| line.select { |key, val| headers.include?(key) } }
  expect(expected).to eq(structured_response), $response.body
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
Around do |scenario, block|
  if $server
    Process.kill "TERM", $server
  end

  $server = fork do
    Dir.mktmpdir do |dir|
      exec 'stack', 'exec', 'hermes', dir + '/hermes.db'
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

