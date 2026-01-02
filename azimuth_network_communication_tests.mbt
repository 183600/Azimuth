// Azimuth Network Communication Test Suite
// This file contains comprehensive test cases for network communication

// Test 1: HTTP Request/Response Simulation
test "http request response simulation" {
  type HTTPRequest {
    method : String
    url : String
    headers : Array[(String, String)]
    body : String
  }
  
  type HTTPResponse {
    status_code : Int
    status_message : String
    headers : Array[(String, String)]
    body : String
  }
  
  let create_request = fn(method, url) {
    { method: method, url: url, headers: [], body: "" }
  }
  
  let add_header = fn(request, key, value) {
    let new_headers = request.headers @ [(key, value)]
    { method: request.method, url: request.url, headers: new_headers, body: request.body }
  }
  
  let set_body = fn(request, body) {
    { method: request.method, url: request.url, headers: request.headers, body: body }
  }
  
  let create_response = fn(status_code, status_message, body) {
    { 
      status_code: status_code, 
      status_message: status_message, 
      headers: [], 
      body: body 
    }
  }
  
  let add_response_header = fn(response, key, value) {
    let new_headers = response.headers @ [(key, value)]
    { 
      status_code: response.status_code, 
      status_message: response.status_message, 
      headers: new_headers, 
      body: response.body 
    }
  }
  
  let simulate_http_request = fn(request) {
    // Simulate different responses based on request
    if request.method == "GET" and request.url == "/api/users" {
      let response = create_response(200, "OK", "[{\"id\": 1, \"name\": \"John\"}]")
      add_response_header(response, "Content-Type", "application/json")
    } else if request.method == "POST" and request.url == "/api/users" {
      let response = create_response(201, "Created", "{\"id\": 2, \"name\": \"Jane\"}")
      add_response_header(response, "Content-Type", "application/json")
    } else if request.url == "/api/notfound" {
      let response = create_response(404, "Not Found", "{\"error\": \"Resource not found\"}")
      add_response_header(response, "Content-Type", "application/json")
    } else {
      let response = create_response(500, "Internal Server Error", "{\"error\": \"Server error\"}")
      add_response_header(response, "Content-Type", "application/json")
    }
  }
  
  // Test HTTP request creation
  let request = create_request("GET", "/api/users")
  let request_with_headers = add_header(add_header(request, "Accept", "application/json"), "User-Agent", "Azimuth/1.0")
  
  assert_eq(request_with_headers.method, "GET")
  assert_eq(request_with_headers.url, "/api/users")
  assert_eq(request_with_headers.headers.length(), 2)
  assert_eq(request_with_headers.headers[0], ("Accept", "application/json"))
  assert_eq(request_with_headers.headers[1], ("User-Agent", "Azimuth/1.0"))
  
  // Test POST request with body
  let post_request = set_body(create_request("POST", "/api/users"), "{\"name\": \"Jane\"}")
  assert_eq(post_request.method, "POST")
  assert_eq(post_request.body, "{\"name\": \"Jane\"}")
  
  // Test HTTP response simulation
  let get_response = simulate_http_request(request_with_headers)
  assert_eq(get_response.status_code, 200)
  assert_eq(get_response.status_message, "OK")
  assert_eq(get_response.body, "[{\"id\": 1, \"name\": \"John\"}]")
  
  // Find Content-Type header
  let mut content_type = ""
  for (key, value) in get_response.headers {
    if key == "Content-Type" {
      content_type = value
      break
    }
  }
  assert_eq(content_type, "application/json")
  
  // Test POST response
  let post_response = simulate_http_request(post_request)
  assert_eq(post_response.status_code, 201)
  assert_eq(post_response.status_message, "Created")
  
  // Test 404 response
  let not_found_request = create_request("GET", "/api/notfound")
  let not_found_response = simulate_http_request(not_found_request)
  assert_eq(not_found_response.status_code, 404)
  assert_eq(not_found_response.status_message, "Not Found")
}

// Test 2: WebSocket Communication Simulation
test "websocket communication simulation" {
  type WebSocketMessage {
    type : String  // "text", "binary", "ping", "pong", "close"
    data : String
    timestamp : Int
  }
  
  type WebSocketConnection {
    id : String
    state : String  // "connecting", "open", "closing", "closed"
    messages_sent : Array[WebSocketMessage]
    messages_received : Array[WebSocketMessage]
  }
  
  let create_connection = fn(id) {
    { 
      id: id, 
      state: "connecting", 
      messages_sent: [], 
      messages_received: [] 
    }
  }
  
  let open_connection = fn(connection) {
    { 
      id: connection.id, 
      state: "open", 
      messages_sent: connection.messages_sent, 
      messages_received: connection.messages_received 
    }
  }
  
  let close_connection = fn(connection) {
    { 
      id: connection.id, 
      state: "closed", 
      messages_sent: connection.messages_sent, 
      messages_received: connection.messages_received 
    }
  }
  
  let send_message = fn(connection, message_type, data) {
    let message = {
      type: message_type,
      data: data,
      timestamp: 1000  // Simulated timestamp
    }
    
    { 
      id: connection.id, 
      state: connection.state, 
      messages_sent: connection.messages_sent @ [message], 
      messages_received: connection.messages_received 
    }
  }
  
  let receive_message = fn(connection, message_type, data) {
    let message = {
      type: message_type,
      data: data,
      timestamp: 1000  // Simulated timestamp
    }
    
    { 
      id: connection.id, 
      state: connection.state, 
      messages_sent: connection.messages_sent, 
      messages_received: connection.messages_received @ [message] 
    }
  }
  
  let simulate_echo_server = fn(connection, message) {
    // Echo server responds with the same message
    if connection.state == "open" and message.type == "text" {
      receive_message(connection, "text", "Echo: " + message.data)
    } else {
      connection
    }
  }
  
  // Test WebSocket connection lifecycle
  let connection = create_connection("conn-123")
  assert_eq(connection.state, "connecting")
  
  let open_conn = open_connection(connection)
  assert_eq(open_conn.state, "open")
  
  // Send messages
  let conn_with_msg1 = send_message(open_conn, "text", "Hello, server!")
  let conn_with_msg2 = send_message(conn_with_msg1, "text", "How are you?")
  
  assert_eq(conn_with_msg2.messages_sent.length(), 2)
  assert_eq(conn_with_msg2.messages_sent[0].data, "Hello, server!")
  assert_eq(conn_with_msg2.messages_sent[1].data, "How are you?")
  
  // Simulate server responses
  let conn_with_resp1 = simulate_echo_server(conn_with_msg2, conn_with_msg2.messages_sent[0])
  let conn_with_resp2 = simulate_echo_server(conn_with_resp1, conn_with_resp1.messages_sent[1])
  
  assert_eq(conn_with_resp2.messages_received.length(), 2)
  assert_eq(conn_with_resp2.messages_received[0].data, "Echo: Hello, server!")
  assert_eq(conn_with_resp2.messages_received[1].data, "Echo: How are you?")
  
  // Close connection
  let closed_conn = close_connection(conn_with_resp2)
  assert_eq(closed_conn.state, "closed")
  
  // Test ping/pong
  let ping_conn = send_message(open_conn, "ping", "")
  assert_eq(ping_conn.messages_sent[2].type, "ping")
  
  let pong_conn = receive_message(ping_conn, "pong", "")
  assert_eq(pong_conn.messages_received[2].type, "pong")
}

// Test 3: TCP Connection Simulation
test "tcp connection simulation" {
  type TCPPacket {
    source_port : Int
    dest_port : Int
    sequence_number : Int
    ack_number : Int
    flags : Array[String]  // "SYN", "ACK", "FIN", "RST"
    data : String
  }
  
  type TCPConnection {
    local_port : Int
    remote_port : Int
    state : String  // "CLOSED", "SYN_SENT", "ESTABLISHED", "FIN_WAIT_1", "CLOSE_WAIT"
    local_sequence : Int
    remote_sequence : Int
    packets_sent : Array[TCPPacket]
    packets_received : Array[TCPPacket]
  }
  
  let create_tcp_connection = fn(local_port, remote_port) {
    { 
      local_port: local_port, 
      remote_port: remote_port, 
      state: "CLOSED", 
      local_sequence: 1000, 
      remote_sequence: 0, 
      packets_sent: [], 
      packets_received: [] 
    }
  }
  
  let create_packet = fn(source_port, dest_port, seq, ack, flags, data) {
    { 
      source_port: source_port, 
      dest_port: dest_port, 
      sequence_number: seq, 
      ack_number: ack, 
      flags: flags, 
      data: data 
    }
  }
  
  let send_packet = fn(connection, flags, data) {
    let packet = create_packet(
      connection.local_port, 
      connection.remote_port, 
      connection.local_sequence, 
      connection.remote_sequence, 
      flags, 
      data
    )
    
    let new_sequence = connection.local_sequence + data.length()
    if flags.contains("SYN") {
      new_sequence = new_sequence + 1
    }
    
    { 
      local_port: connection.local_port, 
      remote_port: connection.remote_port, 
      state: connection.state, 
      local_sequence: new_sequence, 
      remote_sequence: connection.remote_sequence, 
      packets_sent: connection.packets_sent @ [packet], 
      packets_received: connection.packets_received 
    }
  }
  
  let receive_packet = fn(connection, packet) {
    let new_remote_sequence = packet.sequence_number + packet.data.length()
    if packet.flags.contains("SYN") {
      new_remote_sequence = new_remote_sequence + 1
    }
    
    { 
      local_port: connection.local_port, 
      remote_port: connection.remote_port, 
      state: connection.state, 
      local_sequence: connection.local_sequence, 
      remote_sequence: new_remote_sequence, 
      packets_sent: connection.packets_sent, 
      packets_received: connection.packets_received @ [packet] 
    }
  }
  
  // Test TCP three-way handshake
  let client_conn = create_tcp_connection(12345, 80)
  assert_eq(client_conn.state, "CLOSED")
  
  // Client sends SYN
  let client_syn = send_packet(client_conn, ["SYN"], "")
  assert_eq(client_syn.state, "CLOSED")  // State doesn't change until SYN-ACK received
  assert_eq(client_syn.packets_sent.length(), 1)
  assert_true(client_syn.packets_sent[0].flags.contains("SYN"))
  
  // Simulate server SYN-ACK
  let server_syn_ack = create_packet(80, 12345, 2000, 1001, ["SYN", "ACK"], "")
  let client_with_syn_ack = receive_packet(client_syn, server_syn_ack)
  assert_eq(client_with_syn_ack.remote_sequence, 2001)
  
  // Client sends ACK
  let client_ack = send_packet(client_with_syn_ack, ["ACK"], "")
  assert_eq(client_ack.local_sequence, 1001)
  
  // Connection is now established
  let established_conn = { 
    local_port: client_ack.local_port, 
    remote_port: client_ack.remote_port, 
    state: "ESTABLISHED", 
    local_sequence: client_ack.local_sequence, 
    remote_sequence: client_ack.remote_sequence, 
    packets_sent: client_ack.packets_sent, 
    packets_received: client_ack.packets_received 
  }
  
  // Test data transmission
  let data_conn = send_packet(established_conn, ["ACK"], "Hello, server!")
  assert_eq(data_conn.packets_sent.length(), 4)
  assert_eq(data_conn.packets_sent[3].data, "Hello, server!")
  
  // Simulate server response
  let server_response = create_packet(80, 12345, 2001, 1018, ["ACK"], "Hello, client!")
  let client_with_response = receive_packet(data_conn, server_response)
  assert_eq(client_with_response.packets_received.length(), 3)
  assert_eq(client_with_response.packets_received[2].data, "Hello, client!")
}

// Test 4: DNS Resolution Simulation
test "dns resolution simulation" {
  type DNSRecord {
    name : String
    type : String  // "A", "AAAA", "CNAME", "MX", "NS"
    value : String
    ttl : Int
  }
  
  type DNSServer {
    records : Array[DNSRecord]
  }
  
  let create_dns_server = fn() {
    { records: [] }
  }
  
  let add_record = fn(server, name, record_type, value, ttl) {
    let record = { name: name, type: record_type, value: value, ttl: ttl }
    { records: server.records @ [record] }
  }
  
  let query_dns = fn(server, name, record_type) {
    let mut results = []
    
    for record in server.records {
      if record.name == name and record.type == record_type {
        results = results @ [record]
      }
    }
    
    results
  }
  
  let resolve_domain = fn(server, domain) {
    // First try to find A record
    let a_records = query_dns(server, domain, "A")
    
    if a_records.length() > 0 {
      return a_records
    }
    
    // If no A record, look for CNAME
    let cname_records = query_dns(server, domain, "CNAME")
    
    if cname_records.length() > 0 {
      let cname = cname_records[0].value
      // Recursively resolve the CNAME
      resolve_domain(server, cname)
    } else {
      []
    }
  }
  
  // Create DNS server with records
  let dns_server = create_dns_server()
  let dns_server1 = add_record(dns_server, "example.com", "A", "93.184.216.34", 3600)
  let dns_server2 = add_record(dns_server1, "www.example.com", "CNAME", "example.com", 3600)
  let dns_server3 = add_record(dns_server2, "mail.example.com", "A", "192.0.2.1", 3600)
  let dns_server4 = add_record(dns_server3, "example.com", "MX", "mail.example.com", 3600)
  let dns_server5 = add_record(dns_server4, "example.com", "NS", "ns1.example.com", 3600)
  
  // Test DNS queries
  let a_records = query_dns(dns_server5, "example.com", "A")
  assert_eq(a_records.length(), 1)
  assert_eq(a_records[0].value, "93.184.216.34")
  assert_eq(a_records[0].ttl, 3600)
  
  let cname_records = query_dns(dns_server5, "www.example.com", "CNAME")
  assert_eq(cname_records.length(), 1)
  assert_eq(cname_records[0].value, "example.com")
  
  let mx_records = query_dns(dns_server5, "example.com", "MX")
  assert_eq(mx_records.length(), 1)
  assert_eq(mx_records[0].value, "mail.example.com")
  
  // Test domain resolution with CNAME
  let www_records = resolve_domain(dns_server5, "www.example.com")
  assert_eq(www_records.length(), 1)
  assert_eq(www_records[0].value, "93.184.216.34")  // Resolved through CNAME
  
  // Test non-existent domain
  let nonexistent_records = query_dns(dns_server5, "nonexistent.com", "A")
  assert_eq(nonexistent_records.length(), 0)
}

// Test 5: Network Packet Routing Simulation
test "network packet routing simulation" {
  type IPPacket {
    source_ip : String
    dest_ip : String
    ttl : Int
    protocol : String
    data : String
  }
  
  type RoutingTable {
    entries : Array[(String, String, Int)]  // (destination, next_hop, metric)
  }
  
  type Router {
    ip : String
    routing_table : RoutingTable
    packets_received : Array[IPPacket]
    packets_forwarded : Array[IPPacket]
    packets_dropped : Array[IPPacket]
  }
  
  let create_router = fn(ip) {
    { 
      ip: ip, 
      routing_table: { entries: [] }, 
      packets_received: [], 
      packets_forwarded: [], 
      packets_dropped: [] 
    }
  }
  
  let add_route = fn(router, destination, next_hop, metric) {
    let new_entries = router.routing_table.entries @ [(destination, next_hop, metric)]
    let new_routing_table = { entries: new_entries }
    
    { 
      ip: router.ip, 
      routing_table: new_routing_table, 
      packets_received: router.packets_received, 
      packets_forwarded: router.packets_forwarded, 
      packets_dropped: router.packets_dropped 
    }
  }
  
  let create_packet = fn(source_ip, dest_ip, protocol, data) {
    { source_ip: source_ip, dest_ip: dest_ip, ttl: 64, protocol: protocol, data: data }
  }
  
  let find_route = fn(router, destination) {
    let mut best_route = None
    let mut best_metric = 999999
    
    for (dest, next_hop, metric) in router.routing_table.entries {
      if destination.starts_with(dest) and metric < best_metric {
        best_route = Some(next_hop)
        best_metric = metric
      }
    }
    
    best_route
  }
  
  let process_packet = fn(router, packet) {
    let updated_router = { 
      ip: router.ip, 
      routing_table: router.routing_table, 
      packets_received: router.packets_received @ [packet], 
      packets_forwarded: router.packets_forwarded, 
      packets_dropped: router.packets_dropped 
    }
    
    // Check TTL
    if packet.ttl <= 1 {
      // Drop packet
      { 
        ip: updated_router.ip, 
        routing_table: updated_router.routing_table, 
        packets_received: updated_router.packets_received, 
        packets_forwarded: updated_router.packets_forwarded, 
        packets_dropped: updated_router.packets_dropped @ [packet] 
      }
    } else if packet.dest_ip == router.ip {
      // Packet is for this router
      updated_router
    } else {
      // Forward packet
      match find_route(updated_router, packet.dest_ip) {
        Some(next_hop) => {
          let forwarded_packet = { 
            source_ip: packet.source_ip, 
            dest_ip: packet.dest_ip, 
            ttl: packet.ttl - 1, 
            protocol: packet.protocol, 
            data: packet.data 
          }
          
          { 
            ip: updated_router.ip, 
            routing_table: updated_router.routing_table, 
            packets_received: updated_router.packets_received, 
            packets_forwarded: updated_router.packets_forwarded @ [forwarded_packet], 
            packets_dropped: updated_router.packets_dropped 
          }
        }
        None => {
          // No route found, drop packet
          { 
            ip: updated_router.ip, 
            routing_table: updated_router.routing_table, 
            packets_received: updated_router.packets_received, 
            packets_forwarded: updated_router.packets_forwarded, 
            packets_dropped: updated_router.packets_dropped @ [packet] 
          }
        }
      }
    }
  }
  
  // Create routers
  let router1 = create_router("192.168.1.1")
  let router2 = create_router("10.0.0.1")
  let router3 = create_router("172.16.0.1")
  
  // Configure routing tables
  let router1_with_routes = add_route(add_route(router1, "10.0.0.0", "10.0.0.1", 1), "172.16.0.0", "10.0.0.1", 2)
  let router2_with_routes = add_route(add_route(router2, "192.168.1.0", "192.168.1.1", 1), "172.16.0.0", "172.16.0.1", 1)
  let router3_with_routes = add_route(add_route(router3, "192.168.1.0", "10.0.0.1", 2), "10.0.0.0", "10.0.0.1", 1)
  
  // Test packet routing
  let packet1 = create_packet("192.168.1.100", "10.0.0.100", "TCP", "Hello from 192.168.1.100")
  let router1_after_packet1 = process_packet(router1_with_routes, packet1)
  
  assert_eq(router1_after_packet1.packets_received.length(), 1)
  assert_eq(router1_after_packet1.packets_forwarded.length(), 1)
  assert_eq(router1_after_packet1.packets_forwarded[0].dest_ip, "10.0.0.100")
  assert_eq(router1_after_packet1.packets_forwarded[0].ttl, 63)
  
  // Test packet for router
  let packet2 = create_packet("10.0.0.100", "192.168.1.1", "ICMP", "Ping")
  let router1_after_packet2 = process_packet(router1_after_packet1, packet2)
  
  assert_eq(router1_after_packet2.packets_received.length(), 2)
  assert_eq(router1_after_packet2.packets_forwarded.length(), 1)  // No new forwarded packet
  
  // Test packet with no route
  let packet3 = create_packet("192.168.1.100", "203.0.113.100", "TCP", "Hello from 192.168.1.100")
  let router1_after_packet3 = process_packet(router1_after_packet2, packet3)
  
  assert_eq(router1_after_packet3.packets_received.length(), 3)
  assert_eq(router1_after_packet3.packets_dropped.length(), 1)
  
  // Test TTL expiration
  let packet4 = create_packet("192.168.1.100", "10.0.0.100", "TCP", "Hello from 192.168.1.100")
  let packet4_with_low_ttl = { source_ip: packet4.source_ip, dest_ip: packet4.dest_ip, ttl: 1, protocol: packet4.protocol, data: packet4.data }
  let router1_after_packet4 = process_packet(router1_after_packet3, packet4_with_low_ttl)
  
  assert_eq(router1_after_packet4.packets_received.length(), 4)
  assert_eq(router1_after_packet4.packets_dropped.length(), 2)
}

// Test 6: Network Protocol Stack Simulation
test "network protocol stack simulation" {
  type EthernetFrame {
    source_mac : String
    dest_mac : String
    ether_type : Int
    payload : String
  }
  
  type IPPacket {
    source_ip : String
    dest_ip : String
    protocol : Int
    payload : String
  }
  
  type TCPSegment {
    source_port : Int
    dest_port : Int
    sequence_number : Int
    ack_number : Int
    flags : Array[String]
    payload : String
  }
  
  type ProtocolStack {
    ethernet_frames : Array[EthernetFrame]
    ip_packets : Array[IPPacket]
    tcp_segments : Array[TCPSegment]
    application_data : String
  }
  
  let create_stack = fn() {
    { 
      ethernet_frames: [], 
      ip_packets: [], 
      tcp_segments: [], 
      application_data: "" 
    }
  }
  
  let create_ethernet_frame = fn(source_mac, dest_mac, ether_type, payload) {
    { source_mac: source_mac, dest_mac: dest_mac, ether_type: ether_type, payload: payload }
  }
  
  let create_ip_packet = fn(source_ip, dest_ip, protocol, payload) {
    { source_ip: source_ip, dest_ip: dest_ip, protocol: protocol, payload: payload }
  }
  
  let create_tcp_segment = fn(source_port, dest_port, seq, ack, flags, payload) {
    { source_port: source_port, dest_port: dest_port, sequence_number: seq, ack_number: ack, flags: flags, payload: payload }
  }
  
  let encapsulate_tcp = fn(stack, segment) {
    // Create IP packet with TCP segment as payload
    let ip_packet = create_ip_packet("192.168.1.100", "10.0.0.100", 6, segment.payload)
    
    // Create Ethernet frame with IP packet as payload
    let ethernet_frame = create_ethernet_frame("00:11:22:33:44:55", "AA:BB:CC:DD:EE:FF", 0x0800, ip_packet.payload)
    
    { 
      ethernet_frames: stack.ethernet_frames @ [ethernet_frame], 
      ip_packets: stack.ip_packets @ [ip_packet], 
      tcp_segments: stack.tcp_segments @ [segment], 
      application_data: stack.application_data 
    }
  }
  
  let decapsulate_ethernet = fn(stack, frame) {
    // Extract IP packet from Ethernet frame
    let ip_packet = create_ip_packet("192.168.1.100", "10.0.0.100", 6, frame.payload)
    
    // Extract TCP segment from IP packet
    let tcp_segment = create_tcp_segment(12345, 80, 1000, 0, ["SYN"], ip_packet.payload)
    
    { 
      ethernet_frames: stack.ethernet_frames @ [frame], 
      ip_packets: stack.ip_packets @ [ip_packet], 
      tcp_segments: stack.tcp_segments @ [tcp_segment], 
      application_data: tcp_segment.payload 
    }
  }
  
  // Test protocol encapsulation
  let stack = create_stack()
  
  // Create application data
  let app_data = "Hello, world!"
  let tcp_segment = create_tcp_segment(12345, 80, 1000, 0, ["PSH", "ACK"], app_data)
  
  // Encapsulate TCP segment
  let stack_with_data = encapsulate_tcp(stack, tcp_segment)
  
  assert_eq(stack_with_data.tcp_segments.length(), 1)
  assert_eq(stack_with_data.tcp_segments[0].payload, "Hello, world!")
  assert_eq(stack_with_data.ip_packets.length(), 1)
  assert_eq(stack_with_data.ethernet_frames.length(), 1)
  
  // Test protocol decapsulation
  let frame = create_ethernet_frame("AA:BB:CC:DD:EE:FF", "00:11:22:33:44:55", 0x0800, "Hello, world!")
  let stack_with_frame = decapsulate_ethernet(stack_with_data, frame)
  
  assert_eq(stack_with_frame.ethernet_frames.length(), 2)
  assert_eq(stack_with_frame.ip_packets.length(), 2)
  assert_eq(stack_with_frame.tcp_segments.length(), 2)
  assert_eq(stack_with_frame.application_data, "Hello, world!")
}

// Test 7: Network Security Simulation
test "network security simulation" {
  type FirewallRule {
    id : Int
    action : String  // "allow", "deny"
    source_ip : String
    dest_ip : String
    source_port : Int
    dest_port : Int
    protocol : String
  }
  
  type Firewall {
    rules : Array[FirewallRule]
    default_action : String
    packets_allowed : Int
    packets_denied : Int
  }
  
  type IPSPacket {
    source_ip : String
    dest_ip : String
    source_port : Int
    dest_port : Int
    protocol : String
    payload : String
    is_suspicious : Bool
  }
  
  let create_firewall = fn(default_action) {
    { 
      rules: [], 
      default_action: default_action, 
      packets_allowed: 0, 
      packets_denied: 0 
    }
  }
  
  let add_rule = fn(firewall, id, action, source_ip, dest_ip, source_port, dest_port, protocol) {
    let rule = { 
      id: id, 
      action: action, 
      source_ip: source_ip, 
      dest_ip: dest_ip, 
      source_port: source_port, 
      dest_port: dest_port, 
      protocol: protocol 
    }
    
    { 
      rules: firewall.rules @ [rule], 
      default_action: firewall.default_action, 
      packets_allowed: firewall.packets_allowed, 
      packets_denied: firewall.packets_denied 
    }
  }
  
  let create_packet = fn(source_ip, dest_ip, source_port, dest_port, protocol, payload) {
    { 
      source_ip: source_ip, 
      dest_ip: dest_ip, 
      source_port: source_port, 
      dest_port: dest_port, 
      protocol: protocol, 
      payload: payload, 
      is_suspicious: false 
    }
  }
  
  let matches_rule = fn(packet, rule) {
    let ip_matches = (rule.source_ip == "any" or packet.source_ip == rule.source_ip) and
                     (rule.dest_ip == "any" or packet.dest_ip == rule.dest_ip)
    
    let port_matches = (rule.source_port == 0 or packet.source_port == rule.source_port) and
                       (rule.dest_port == 0 or packet.dest_port == rule.dest_port)
    
    let protocol_matches = rule.protocol == "any" or packet.protocol == rule.protocol
    
    ip_matches and port_matches and protocol_matches
  }
  
  let process_packet = fn(firewall, packet) {
    let mut action = firewall.default_action
    
    // Check rules in order
    for rule in firewall.rules {
      if matches_rule(packet, rule) {
        action = rule.action
        break
      }
    }
    
    if action == "allow" {
      { 
        rules: firewall.rules, 
        default_action: firewall.default_action, 
        packets_allowed: firewall.packets_allowed + 1, 
        packets_denied: firewall.packets_denied 
      }
    } else {
      { 
        rules: firewall.rules, 
        default_action: firewall.default_action, 
        packets_allowed: firewall.packets_allowed, 
        packets_denied: firewall.packets_denied + 1 
      }
    }
  }
  
  let detect_intrusion = fn(packet) {
    // Simple intrusion detection
    let mut suspicious = false
    
    // Check for port scanning (connection to many different ports)
    if packet.dest_port >= 1 and packet.dest_port <= 1024 and packet.payload == "" {
      suspicious = true
    }
    
    // Check for suspicious payload
    if packet.payload.contains("../") or packet.payload.contains("<script>") {
      suspicious = true
    }
    
    { 
      source_ip: packet.source_ip, 
      dest_ip: packet.dest_ip, 
      source_port: packet.source_port, 
      dest_port: packet.dest_port, 
      protocol: packet.protocol, 
      payload: packet.payload, 
      is_suspicious: suspicious 
    }
  }
  
  // Create firewall
  let firewall = create_firewall("deny")
  
  // Add rules
  let firewall_with_rules = add_rule(
    add_rule(
      add_rule(firewall, 1, "allow", "192.168.1.0", "any", 0, 0, "any"),  // Allow internal network
      2, "allow", "any", "any", 0, 80, "TCP"  // Allow HTTP
    ),
    3, "allow", "any", "any", 0, 443, "TCP"  // Allow HTTPS
  )
  
  // Test packet processing
  let internal_packet = create_packet("192.168.1.100", "10.0.0.100", 12345, 80, "TCP", "GET /index.html")
  let firewall_after_internal = process_packet(firewall_with_rules, internal_packet)
  
  assert_eq(firewall_after_internal.packets_allowed, 1)
  assert_eq(firewall_after_internal.packets_denied, 0)
  
  let external_packet = create_packet("203.0.113.100", "192.168.1.100", 12345, 22, "TCP", "SSH connection")
  let firewall_after_external = process_packet(firewall_after_internal, external_packet)
  
  assert_eq(firewall_after_external.packets_allowed, 1)
  assert_eq(firewall_after_external.packets_denied, 1)
  
  let http_packet = create_packet("203.0.113.100", "192.168.1.100", 12345, 80, "TCP", "GET /index.html")
  let firewall_after_http = process_packet(firewall_after_external, http_packet)
  
  assert_eq(firewall_after_http.packets_allowed, 2)
  assert_eq(firewall_after_http.packets_denied, 1)
  
  // Test intrusion detection
  let suspicious_packet = create_packet("203.0.113.100", "192.168.1.100", 12345, 22, "TCP", "")
  let detected_packet = detect_intrusion(suspicious_packet)
  
  assert_true(detected_packet.is_suspicious)
  
  let malicious_packet = create_packet("203.0.113.100", "192.168.1.100", 12345, 80, "TCP", "GET /../../etc/passwd")
  let detected_malicious = detect_intrusion(malicious_packet)
  
  assert_true(detected_malicious.is_suspicious)
  
  let normal_packet = create_packet("203.0.113.100", "192.168.1.100", 12345, 80, "TCP", "GET /index.html")
  let detected_normal = detect_intrusion(normal_packet)
  
  assert_false(detected_normal.is_suspicious)
}

// Test 8: Network Load Balancing Simulation
test "network load balancing simulation" {
  type Server {
    id : String
    ip : String
    port : Int
    weight : Int
    current_connections : Int
    total_requests : Int
    is_healthy : Bool
  }
  
  type LoadBalancer {
    algorithm : String  // "round_robin", "weighted", "least_connections"
    servers : Array[Server]
    current_index : Int
    total_requests : Int
  }
  
  let create_server = fn(id, ip, port, weight) {
    { 
      id: id, 
      ip: ip, 
      port: port, 
      weight: weight, 
      current_connections: 0, 
      total_requests: 0, 
      is_healthy: true 
    }
  }
  
  let create_load_balancer = fn(algorithm) {
    { 
      algorithm: algorithm, 
      servers: [], 
      current_index: 0, 
      total_requests: 0 
    }
  }
  
  let add_server = fn(lb, server) {
    { 
      algorithm: lb.algorithm, 
      servers: lb.servers @ [server], 
      current_index: lb.current_index, 
      total_requests: lb.total_requests 
    }
  }
  
  let select_server_round_robin = fn(lb) {
    if lb.servers.length() == 0 {
      None
    } else {
      let server = lb.servers[lb.current_index]
      let next_index = (lb.current_index + 1) % lb.servers.length()
      
      Some((server, next_index))
    }
  }
  
  let select_server_weighted = fn(lb) {
    if lb.servers.length() == 0 {
      None
    } else {
      // Calculate total weight
      let mut total_weight = 0
      for server in lb.servers {
        if server.is_healthy {
          total_weight = total_weight + server.weight
        }
      }
      
      if total_weight == 0 {
        None
      } else {
        // Simple weighted selection (simplified)
        let mut weighted_servers = []
        for server in lb.servers {
          if server.is_healthy {
            let mut i = 0
            while i < server.weight {
              weighted_servers = weighted_servers @ [server]
              i = i + 1
            }
          }
        }
        
        let index = lb.total_requests % weighted_servers.length()
        Some((weighted_servers[index], lb.current_index))
      }
    }
  }
  
  let select_server_least_connections = fn(lb) {
    if lb.servers.length() == 0 {
      None
    } else {
      let mut selected_server = lb.servers[0]
      let mut min_connections = selected_server.current_connections
      
      for server in lb.servers {
        if server.is_healthy and server.current_connections < min_connections {
          selected_server = server
          min_connections = server.current_connections
        }
      }
      
      Some((selected_server, lb.current_index))
    }
  }
  
  let select_server = fn(lb) {
    match lb.algorithm {
      "round_robin" => select_server_round_robin(lb)
      "weighted" => select_server_weighted(lb)
      "least_connections" => select_server_least_connections(lb)
      _ => None
    }
  }
  
  let process_request = fn(lb) {
    match select_server(lb) {
      Some((server, next_index)) => {
        // Update server stats
        let mut updated_servers = []
        for s in lb.servers {
          if s.id == server.id {
            updated_servers = updated_servers @ [{
              id: s.id, 
              ip: s.ip, 
              port: s.port, 
              weight: s.weight, 
              current_connections: s.current_connections + 1, 
              total_requests: s.total_requests + 1, 
              is_healthy: s.is_healthy 
            }]
          } else {
            updated_servers = updated_servers @ [s]
          }
        }
        
        { 
          algorithm: lb.algorithm, 
          servers: updated_servers, 
          current_index: next_index, 
          total_requests: lb.total_requests + 1 
        }
      }
      None => lb
    }
  }
  
  let release_connection = fn(lb, server_id) {
    let mut updated_servers = []
    
    for server in lb.servers {
      if server.id == server_id {
        updated_servers = updated_servers @ [{
          id: server.id, 
          ip: server.ip, 
          port: server.port, 
          weight: server.weight, 
          current_connections: server.current_connections - 1, 
          total_requests: server.total_requests, 
          is_healthy: server.is_healthy 
        }]
      } else {
        updated_servers = updated_servers @ [server]
      }
    }
    
    { 
      algorithm: lb.algorithm, 
      servers: updated_servers, 
      current_index: lb.current_index, 
      total_requests: lb.total_requests 
    }
  }
  
  // Create servers
  let server1 = create_server("server1", "192.168.1.10", 8080, 1)
  let server2 = create_server("server2", "192.168.1.11", 8080, 2)
  let server3 = create_server("server3", "192.168.1.12", 8080, 3)
  
  // Create load balancer with round-robin algorithm
  let lb = create_load_balancer("round_robin")
  let lb_with_servers = add_server(add_server(add_server(lb, server1), server2), server3)
  
  // Test round-robin load balancing
  let lb1 = process_request(lb_with_servers)
  let lb2 = process_request(lb1)
  let lb3 = process_request(lb2)
  let lb4 = process_request(lb3)
  
  assert_eq(lb1.servers[0].current_connections, 1)  // server1
  assert_eq(lb2.servers[1].current_connections, 1)  // server2
  assert_eq(lb3.servers[2].current_connections, 1)  // server3
  assert_eq(lb4.servers[0].current_connections, 2)  // back to server1
  
  // Test weighted load balancing
  let weighted_lb = create_load_balancer("weighted")
  let weighted_lb_with_servers = add_server(add_server(add_server(weighted_lb, server1), server2), server3)
  
  let weighted_lb1 = process_request(weighted_lb_with_servers)
  let weighted_lb2 = process_request(weighted_lb1)
  let weighted_lb3 = process_request(weighted_lb2)
  let weighted_lb4 = process_request(weighted_lb3)
  let weighted_lb5 = process_request(weighted_lb4)
  let weighted_lb6 = process_request(weighted_lb5)
  
  // With weights 1, 2, 3, server3 should get more requests
  assert_eq(weighted_lb1.servers[2].total_requests, 1)  // server3 (weight 3)
  assert_eq(weighted_lb2.servers[2].total_requests, 2)  // server3 (weight 3)
  assert_eq(weighted_lb3.servers[1].total_requests, 1)  // server2 (weight 2)
  assert_eq(weighted_lb4.servers[2].total_requests, 3)  // server3 (weight 3)
  assert_eq(weighted_lb5.servers[1].total_requests, 2)  // server2 (weight 2)
  assert_eq(weighted_lb6.servers[0].total_requests, 1)  // server1 (weight 1)
  
  // Test least connections load balancing
  let lc_lb = create_load_balancer("least_connections")
  let lc_lb_with_servers = add_server(add_server(add_server(lc_lb, server1), server2), server3)
  
  // Simulate existing connections
  let lc_lb_with_connections = { 
    algorithm: "least_connections", 
    servers: [
      { id: "server1", ip: "192.168.1.10", port: 8080, weight: 1, current_connections: 5, total_requests: 10, is_healthy: true },
      { id: "server2", ip: "192.168.1.11", port: 8080, weight: 2, current_connections: 3, total_requests: 8, is_healthy: true },
      { id: "server3", ip: "192.168.1.12", port: 8080, weight: 3, current_connections: 1, total_requests: 5, is_healthy: true }
    ], 
    current_index: 0, 
    total_requests: 23 
  }
  
  let lc_lb1 = process_request(lc_lb_with_connections)
  
  // Should select server3 (least connections)
  assert_eq(lc_lb1.servers[2].current_connections, 2)
  
  // Release connections
  let lc_lb_released = release_connection(lc_lb1, "server3")
  assert_eq(lc_lb_released.servers[2].current_connections, 1)
}

// Test 9: Network Latency Simulation
test "network latency simulation" {
  type NetworkPath {
    source : String
    destination : String
    hops : Array[String]
    latencies : Array[Int]  // Latency for each hop in ms
  }
  
  type NetworkMeasurement {
    path : NetworkPath
    total_latency : Int
    jitter : Int
    packet_loss : Float
  }
  
  let create_path = fn(source, destination, hops) {
    let mut latencies = []
    for i in 0..hops.length() {
      latencies = latencies @ [(i + 1) * 10]  // Simulate increasing latency
    }
    
    { 
      source: source, 
      destination: destination, 
      hops: hops, 
      latencies: latencies 
    }
  }
  
  let measure_latency = fn(path, sample_size) {
    // Simulate latency measurements with jitter
    let mut measurements = []
    
    for i in 0..sample_size {
      let mut total_latency = 0
      
      for base_latency in path.latencies {
        // Add random jitter (-5ms to +5ms)
        let jitter = (i * 7) % 11 - 5  // Simplified pseudo-random jitter
        total_latency = total_latency + (base_latency + jitter)
      }
      
      measurements = measurements @ [total_latency]
    }
    
    // Calculate statistics
    let mut sum = 0
    let mut min = measurements[0]
    let mut max = measurements[0]
    
    for measurement in measurements {
      sum = sum + measurement
      if measurement < min { min = measurement }
      if measurement > max { max = measurement }
    }
    
    let avg = sum / measurements.length()
    let jitter = max - min
    
    // Simulate packet loss (1% for this example)
    let packet_loss = 0.01
    
    { 
      path: path, 
      total_latency: avg, 
      jitter: jitter, 
      packet_loss: packet_loss 
    }
  }
  
  let simulate_congestion = fn(path, congestion_factor) {
    let mut new_latencies = []
    
    for latency in path.latencies {
      // Increase latency based on congestion factor
      let new_latency = latency + (latency * congestion_factor / 100)
      new_latencies = new_latencies @ [new_latency.to_int()]
    }
    
    { 
      source: path.source, 
      destination: path.destination, 
      hops: path.hops, 
      latencies: new_latencies 
    }
  }
  
  // Create network paths
  let path1 = create_path("Client", "Server", ["Router1", "Router2", "Router3"])
  let path2 = create_path("Client", "Server", ["RouterA", "RouterB"])
  
  // Measure latency
  let measurement1 = measure_latency(path1, 10)
  let measurement2 = measure_latency(path2, 10)
  
  // Verify measurements
  assert_eq(measurement1.path.hops.length(), 3)
  assert_eq(measurement1.path.latencies.length(), 3)
  assert_true(measurement1.total_latency > 0)
  assert_true(measurement1.jitter >= 0)
  assert_eq(measurement1.packet_loss, 0.01)
  
  assert_eq(measurement2.path.hops.length(), 2)
  assert_eq(measurement2.path.latencies.length(), 2)
  assert_true(measurement2.total_latency > 0)
  assert_true(measurement2.jitter >= 0)
  
  // Path with fewer hops should have lower latency
  assert_true(measurement2.total_latency < measurement1.total_latency)
  
  // Simulate network congestion
  let congested_path1 = simulate_congestion(path1, 50)  // 50% increase
  let congested_measurement1 = measure_latency(congested_path1, 10)
  
  // Congested path should have higher latency
  assert_true(congested_measurement1.total_latency > measurement1.total_latency)
  
  // Verify congestion effect
  for i in 0..path1.latencies.length() {
    assert_true(congested_path1.latencies[i] > path1.latencies[i])
  }
}

// Test 10: Network Bandwidth Simulation
test "network bandwidth simulation" {
  type NetworkLink {
    source : String
    destination : String
    bandwidth : Int  // in Mbps
    latency : Int    // in ms
    utilization : Float  // 0.0 to 1.0
  }
  
  type DataTransfer {
    size : Int        // in MB
    start_time : Int  // in ms
    end_time : Int    // in ms
    throughput : Float  // in Mbps
  }
  
  let create_link = fn(source, destination, bandwidth, latency) {
    { 
      source: source, 
      destination: destination, 
      bandwidth: bandwidth, 
      latency: latency, 
      utilization: 0.0 
    }
  }
  
  let calculate_transfer_time = fn(link, data_size) {
    // Calculate effective bandwidth considering utilization
    let effective_bandwidth = link.bandwidth * (1.0 - link.utilization)
    
    // Transfer time = (data size * 8) / (bandwidth in Mbps) + latency
    // Convert MB to Mb: data_size * 8
    let transfer_time = ((data_size * 8).to_float() / effective_bandwidth).to_int() + link.latency
    
    transfer_time
  }
  
  let simulate_transfer = fn(link, data_size, start_time) {
    let transfer_time = calculate_transfer_time(link, data_size)
    let end_time = start_time + transfer_time
    
    // Calculate throughput: (data size * 8) / transfer time
    let throughput = (data_size * 8).to_float() / transfer_time.to_float()
    
    { 
      size: data_size, 
      start_time: start_time, 
      end_time: end_time, 
      throughput: throughput 
    }
  }
  
  let update_utilization = fn(link, utilization) {
    { 
      source: link.source, 
      destination: link.destination, 
      bandwidth: link.bandwidth, 
      latency: link.latency, 
      utilization: utilization 
    }
  }
  
  let simulate_multiple_transfers = fn(link, data_sizes) {
    let mut transfers = []
    let mut current_time = 0
    let mut current_link = link
    
    for data_size in data_sizes {
      let transfer = simulate_transfer(current_link, data_size, current_time)
      transfers = transfers @ [transfer]
      
      // Update utilization based on this transfer
      let transfer_duration = transfer.end_time - transfer.start_time
      let utilization = (data_size * 8).to_float() / (link.bandwidth * transfer_duration.to_float())
      current_link = update_utilization(current_link, utilization)
      
      current_time = transfer.end_time
    }
    
    transfers
  }
  
  // Create network links
  let fast_link = create_link("Client", "Server", 100, 10)  // 100 Mbps, 10ms latency
  let slow_link = create_link("Client", "Server", 10, 50)   // 10 Mbps, 50ms latency
  
  // Test single data transfer
  let transfer1 = simulate_transfer(fast_link, 100, 0)  // 100 MB
  let transfer2 = simulate_transfer(slow_link, 100, 0)  // 100 MB
  
  // Verify transfer calculations
  assert_eq(transfer1.size, 100)
  assert_eq(transfer1.start_time, 0)
  assert_true(transfer1.end_time > 0)
  assert_true(transfer1.throughput > 0)
  
  assert_eq(transfer2.size, 100)
  assert_eq(transfer2.start_time, 0)
  assert_true(transfer2.end_time > transfer1.end_time)  // Slower link takes longer
  assert_true(transfer2.throughput < transfer1.throughput)  // Lower throughput
  
  // Test multiple transfers
  let data_sizes = [10, 20, 50, 100]  // MB
  let transfers = simulate_multiple_transfers(fast_link, data_sizes)
  
  assert_eq(transfers.length(), 4)
  
  // Verify sequential transfers
  for i in 0..transfers.length() - 1 {
    assert_eq(transfers[i].end_time, transfers[i + 1].start_time)
  }
  
  // Later transfers should be affected by increased utilization
  assert_true(transfers[3].throughput <= transfers[0].throughput)
  
  // Test with different utilization levels
  let busy_link = update_utilization(fast_link, 0.5)  // 50% utilization
  let busy_transfer = simulate_transfer(busy_link, 100, 0)
  
  // Busy link should have lower throughput
  assert_true(busy_transfer.throughput < transfer1.throughput)
  assert_true(busy_transfer.end_time > transfer1.end_time)
}