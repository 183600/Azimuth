// Azimuth Distributed System Consistency Comprehensive Test Suite
// This file contains comprehensive tests for distributed system consistency in the Azimuth telemetry system

// Test 1: Eventual Consistency Model
test "eventual consistency model" {
  // Node and data structure definitions
  type Node = {
    id: String,
    data: Array[(String, String)],
    timestamp: Int,
    vector_clock: Array[String]
  }
  
  type UpdateOperation = {
    key: String,
    value: String,
    node_id: String,
    timestamp: Int,
    vector_clock: Array[String]
  }
  
  // Create initial nodes
  let create_node = fn(id: String) {
    {
      id,
      data: [],
      timestamp: 1640995200,
      vector_clock: []
    }
  }
  
  let initialize_vector_clock = fn(nodes: Array[Node]) {
    let mut initialized_nodes = []
    
    for node in nodes {
      let mut vector_clock = []
      for n in nodes {
        vector_clock = vector_clock.push("0")
      }
      
      initialized_nodes = initialized_nodes.push({
        id: node.id,
        data: node.data,
        timestamp: node.timestamp,
        vector_clock
      })
    }
    
    initialized_nodes
  }
  
  // Test vector clock initialization
  let nodes = [
    create_node("node-1"),
    create_node("node-2"),
    create_node("node-3")
  ]
  
  let initialized_nodes = initialize_vector_clock(nodes)
  assert_eq(initialized_nodes.length(), 3)
  
  for node in initialized_nodes {
    assert_eq(node.vector_clock.length(), 3)
    for clock in node.vector_clock {
      assert_eq(clock, "0")
    }
  }
  
  // Update operation with vector clock
  let apply_update = fn(node: Node, update: UpdateOperation) {
    // Check if update should be applied based on vector clock
    let mut should_apply = true
    let mut updated_vector_clock = node.vector_clock
    
    for i in 0..node.vector_clock.length() {
      let node_clock = node.vector_clock[i].to_int()
      let update_clock = update.vector_clock[i].to_int()
      
      if update_clock < node_clock {
        should_apply = false
        break
      }
      
      if update_clock > node_clock {
        updated_vector_clock[i] = update_clock
      }
    }
    
    if should_apply {
      // Apply the update
      let mut updated_data = []
      let mut found = false
      
      for item in node.data {
        if item.0 == update.key {
          updated_data = updated_data.push((update.key, update.value))
          found = true
        } else {
          updated_data = updated_data.push(item)
        }
      }
      
      if not(found) {
        updated_data = updated_data.push((update.key, update.value))
      }
      
      {
        id: node.id,
        data: updated_data,
        timestamp: update.timestamp,
        vector_clock: updated_vector_clock
      }
    } else {
      // Don't apply the update
      node
    }
  }
  
  // Create update operation
  let create_update = fn(key: String, value: String, node_id: String, vector_clock: Array[String]) {
    let timestamp = 1640995200 + vector_clock.reduce(fn(acc, clock) { acc + clock.to_int() }, 0)
    
    {
      key,
      value,
      node_id,
      timestamp,
      vector_clock
    }
  }
  
  // Increment vector clock for a node
  let increment_vector_clock = fn(vector_clock: Array[String], node_index: Int) {
    let mut updated_clock = []
    
    for i in 0..vector_clock.length() {
      if i == node_index {
        updated_clock = updated_clock.push((vector_clock[i].to_int() + 1).to_string())
      } else {
        updated_clock = updated_clock.push(vector_clock[i])
      }
    }
    
    updated_clock
  }
  
  // Test applying updates
  let node_1 = initialized_nodes[0]
  let node_2 = initialized_nodes[1]
  let node_3 = initialized_nodes[2]
  
  // Node 1 creates an update
  let node_1_clock = increment_vector_clock(node_1.vector_clock, 0)
  let update_1 = create_update("key-1", "value-1", "node-1", node_1_clock)
  
  // Apply update to node 1
  let updated_node_1 = apply_update(node_1, update_1)
  assert_eq(updated_node_1.data.length(), 1)
  assert_eq(updated_node_1.data[0], ("key-1", "value-1"))
  
  // Node 2 creates a different update
  let node_2_clock = increment_vector_clock(node_2.vector_clock, 1)
  let update_2 = create_update("key-2", "value-2", "node-2", node_2_clock)
  
  // Apply update to node 2
  let updated_node_2 = apply_update(node_2, update_2)
  assert_eq(updated_node_2.data.length(), 1)
  assert_eq(updated_node_2.data[0], ("key-2", "value-2"))
  
  // Replicate updates between nodes
  let replicate_update = fn(source_node: Node, target_node: Node, key: String) {
    let mut update = None
    
    for item in source_node.data {
      if item.0 == key {
        update = Some(create_update(item.0, item.1, source_node.id, source_node.vector_clock))
        break
      }
    }
    
    match update {
      Some(u) => apply_update(target_node, u)
      None => target_node
    }
  }
  
  // Replicate update from node 1 to node 2
  let replicated_node_2 = replicate_update(updated_node_1, updated_node_2, "key-1")
  assert_eq(replicated_node_2.data.length(), 2)
  assert_true(replicated_node_2.data.contains(("key-1", "value-1")))
  assert_true(replicated_node_2.data.contains(("key-2", "value-2")))
  
  // Replicate update from node 2 to node 1
  let replicated_node_1 = replicate_update(updated_node_2, updated_node_1, "key-2")
  assert_eq(replicated_node_1.data.length(), 2)
  assert_true(replicated_node_1.data.contains(("key-1", "value-1")))
  assert_true(replicated_node_1.data.contains(("key-2", "value-2")))
  
  // Test conflict resolution
  let node_3_clock = increment_vector_clock(node_3.vector_clock, 2)
  let update_3 = create_update("key-1", "value-3-conflict", "node-3", node_3_clock)
  
  // Apply conflicting update to node 3
  let updated_node_3 = apply_update(node_3, update_3)
  assert_eq(updated_node_3.data.length(), 1)
  assert_eq(updated_node_3.data[0], ("key-1", "value-3-conflict"))
  
  // Now replicate all updates to all nodes to test conflict resolution
  let fully_replicated_node_1 = replicate_update(replicated_node_1, updated_node_3, "key-1")
  let fully_replicated_node_2 = replicate_update(replicated_node_2, updated_node_3, "key-1")
  
  // Verify eventual consistency (all nodes should have the same data)
  assert_eq(fully_replicated_node_1.data.length(), fully_replicated_node_2.data.length())
  
  for item in fully_replicated_node_1.data {
    assert_true(fully_replicated_node_2.data.contains(item))
  }
}

// Test 2: Quorum-based Consistency
test "quorum-based consistency" {
  // Quorum configuration
  type QuorumConfig = {
    total_nodes: Int,
    read_quorum: Int,
    write_quorum: Int
  }
  
  type NodeState = {
    id: String,
    data: Array[(String, String)],
    version: Int,
    timestamp: Int
  }
  
  type ReadResult = {
    value: Option[String],
    version: Int,
    success_nodes: Int
  }
  
  type WriteResult = {
    success: Bool,
    success_nodes: Int
  }
  
  // Create quorum configuration
  let create_quorum_config = fn(total_nodes: Int) {
    let read_quorum = (total_nodes / 2) + 1
    let write_quorum = (total_nodes / 2) + 1
    
    {
      total_nodes,
      read_quorum,
      write_quorum
    }
  }
  
  // Test quorum configuration
  let quorum_config = create_quorum_config(5)
  assert_eq(quorum_config.total_nodes, 5)
  assert_eq(quorum_config.read_quorum, 3)
  assert_eq(quorum_config.write_quorum, 3)
  
  // Create nodes
  let create_nodes = fn(count: Int) {
    let mut nodes = []
    
    for i in 0..count {
      nodes = nodes.push({
        id: "node-" + i.to_string(),
        data: [],
        version: 0,
        timestamp: 1640995200
      })
    }
    
    nodes
  }
  
  let nodes = create_nodes(5)
  assert_eq(nodes.length(), 5)
  
  // Read operation with quorum
  let read_with_quorum = fn(nodes: Array[NodeState>, key: String, quorum_config: QuorumConfig) {
    let mut responses = []
    
    // Collect responses from all nodes
    for node in nodes {
      let mut found = false
      let mut value = None
      let mut version = 0
      
      for item in node.data {
        if item.0 == key {
          found = true
          value = Some(item.1)
          version = node.version
          break
        }
      }
      
      responses = responses.push({
        node_id: node.id,
        value,
        version,
        timestamp: node.timestamp
      })
    }
    
    // Sort by version (highest first)
    responses.sort(fn(a, b) { if a.version > b.version { -1 } else if a.version < b.version { 1 } else { 0 } })
    
    // Check if we have enough responses for read quorum
    if responses.length() >= quorum_config.read_quorum {
      let latest_value = responses[0].value
      let latest_version = responses[0].version
      
      {
        value: latest_value,
        version: latest_version,
        success_nodes: responses.length()
      }
    } else {
      {
        value: None,
        version: 0,
        success_nodes: responses.length()
      }
    }
  }
  
  // Write operation with quorum
  let write_with_quorum = fn(nodes: Array[NodeState], key: String, value: String, quorum_config: QuorumConfig) {
    let mut updated_nodes = []
    let mut success_count = 0
    
    // Try to write to all nodes
    for node in nodes {
      let mut found = false
      let mut updated_data = []
      
      for item in node.data {
        if item.0 == key {
          updated_data = updated_data.push((key, value))
          found = true
        } else {
          updated_data = updated_data.push(item)
        }
      }
      
      if not(found) {
        updated_data = updated_data.push((key, value))
      }
      
      updated_nodes = updated_nodes.push({
        id: node.id,
        data: updated_data,
        version: node.version + 1,
        timestamp: 1640995200 + node.version + 1
      })
      
      success_count = success_count + 1
    }
    
    {
      success: success_count >= quorum_config.write_quorum,
      success_nodes: success_count
    }
  }
  
  // Test read operation (should return None for non-existent key)
  let read_result = read_with_quorum(nodes, "test-key", quorum_config)
  assert_eq(read_result.value, None)
  assert_eq(read_result.version, 0)
  assert_eq(read_result.success_nodes, 5)
  
  // Test write operation
  let write_result = write_with_quorum(nodes, "test-key", "test-value", quorum_config)
  assert_true(write_result.success)
  assert_eq(write_result.success_nodes, 5)
  
  // Apply the write to nodes
  let mut written_nodes = []
  for node in nodes {
    let mut found = false
    let mut updated_data = []
    
    for item in node.data {
      if item.0 == "test-key" {
        updated_data = updated_data.push(("test-key", "test-value"))
        found = true
      } else {
        updated_data = updated_data.push(item)
      }
    }
    
    if not(found) {
      updated_data = updated_data.push(("test-key", "test-value"))
    }
    
    written_nodes = written_nodes.push({
      id: node.id,
      data: updated_data,
      version: node.version + 1,
      timestamp: 1640995201
    })
  }
  
  // Test read operation after write
  let read_result_after_write = read_with_quorum(written_nodes, "test-key", quorum_config)
  assert_eq(read_result_after_write.value, Some("test-value"))
  assert_eq(read_result_after_write.version, 1)
  assert_eq(read_result_after_write.success_nodes, 5)
  
  // Test read repair (ensuring all nodes have the latest data)
  let read_repair = fn(nodes: Array[NodeState>, key: String, quorum_config: QuorumConfig) {
    let read_result = read_with_quorum(nodes, key, quorum_config)
    
    match read_result.value {
      Some(value) => {
        let mut repaired_nodes = []
        
        for node in nodes {
          let mut needs_repair = false
          
          // Check if node has the latest value
          for item in node.data {
            if item.0 == key {
              if item.1 != value || node.version < read_result.version {
                needs_repair = true
              }
              break
            }
          }
          
          // If key doesn't exist or needs repair, update it
          if needs_repair || not(nodes.any(fn(n) { n.data.any(fn(item) { item.0 == key }) })) {
            let mut updated_data = []
            let mut found = false
            
            for item in node.data {
              if item.0 == key {
                updated_data = updated_data.push((key, value))
                found = true
              } else {
                updated_data = updated_data.push(item)
              }
            }
            
            if not(found) {
              updated_data = updated_data.push((key, value))
            }
            
            repaired_nodes = repaired_nodes.push({
              id: node.id,
              data: updated_data,
              version: read_result.version,
              timestamp: 1640995200 + read_result.version
            })
          } else {
            repaired_nodes = repaired_nodes.push(node)
          }
        }
        
        repaired_nodes
      }
      None => nodes
    }
  }
  
  // Create a scenario where some nodes have stale data
  let mut stale_nodes = []
  for i in 0..5 {
    let node = written_nodes[i]
    
    if i < 2 {
      // First two nodes have stale data
      let mut stale_data = []
      for item in node.data {
        if item.0 == "test-key" {
          stale_data = stale_data.push(("test-key", "old-value"))
        } else {
          stale_data = stale_data.push(item)
        }
      }
      
      stale_nodes = stale_nodes.push({
        id: node.id,
        data: stale_data,
        version: 0,  // Stale version
        timestamp: 1640995000  // Stale timestamp
      })
    } else {
      stale_nodes = stale_nodes.push(node)
    }
  }
  
  // Verify some nodes have stale data
  let stale_read = read_with_quorum(stale_nodes, "test-key", quorum_config)
  assert_eq(stale_read.value, Some("test-value"))  // Should still get latest value due to quorum
  assert_eq(stale_read.version, 1)
  
  // Perform read repair
  let repaired_nodes = read_repair(stale_nodes, "test-key", quorum_config)
  
  // Verify all nodes now have the latest data
  for node in repaired_nodes {
    let mut found = false
    for item in node.data {
      if item.0 == "test-key" {
        assert_eq(item.1, "test-value")
        assert_eq(node.version, 1)
        found = true
        break
      }
    }
    assert_true(found)
  }
  
  // Test quorum failure scenarios
  let create_partial_nodes = fn(available_nodes: Int) {
    let mut partial_nodes = []
    
    for i in 0..available_nodes {
      partial_nodes = partial_nodes.push(written_nodes[i])
    }
    
    partial_nodes
  }
  
  // Test with insufficient nodes for read quorum
  let partial_read_nodes = create_partial_nodes(2)  // Less than read quorum
  let partial_read_result = read_with_quorum(partial_read_nodes, "test-key", quorum_config)
  assert_eq(partial_read_result.value, None)  // Should fail due to insufficient nodes
  assert_eq(partial_read_result.success_nodes, 2)
  
  // Test with insufficient nodes for write quorum
  let partial_write_nodes = create_partial_nodes(2)  // Less than write quorum
  let partial_write_result = write_with_quorum(partial_write_nodes, "new-key", "new-value", quorum_config)
  assert_false(partial_write_result.success)  // Should fail due to insufficient nodes
  assert_eq(partial_write_result.success_nodes, 2)
}

// Test 3: Leader Election and Consensus
test "leader election and consensus" {
  // Node state for leader election
  type NodeState = {
    id: String,
    term: Int,
    voted_for: Option[String],
    is_leader: Bool,
    log: Array[(Int, String)]
  }
  
  type VoteRequest = {
    term: Int,
    candidate_id: String,
    last_log_index: Int,
    last_log_term: Int
  }
  
  type VoteResponse = {
    term: Int,
    vote_granted: Bool
  }
  
  // Create initial nodes
  let create_consensus_nodes = fn(count: Int) {
    let mut nodes = []
    
    for i in 0..count {
      nodes = nodes.push({
        id: "node-" + i.to_string(),
        term: 0,
        voted_for: None,
        is_leader: false,
        log: []
      })
    }
    
    nodes
  }
  
  let consensus_nodes = create_consensus_nodes(5)
  assert_eq(consensus_nodes.length(), 5)
  
  // Leader election simulation
  let start_election = fn(nodes: Array[NodeState], candidate_id: String) {
    let mut updated_nodes = []
    let candidate_term = match nodes.find(fn(n) { n.id == candidate_id }) {
      Some(node) => node.term + 1,
      None => 1
    }
    
    for node in nodes {
      if node.id == candidate_id {
        // Candidate votes for itself
        updated_nodes = updated_nodes.push({
          id: node.id,
          term: candidate_term,
          voted_for: Some(candidate_id),
          is_leader: false,
          log: node.log
        })
      } else {
        // Other nodes evaluate the vote request
        let vote_granted = node.term < candidate_term || 
                         (node.term == candidate_term && node.voted_for == None)
        
        let new_voted_for = if vote_granted { Some(candidate_id) } else { node.voted_for }
        
        updated_nodes = updated_nodes.push({
          id: node.id,
          term: node.term,
          voted_for: new_voted_for,
          is_leader: false,
          log: node.log
        })
      }
    }
    
    updated_nodes
  }
  
  // Count votes for a candidate
  let count_votes = fn(nodes: Array[NodeState>, candidate_id: String) {
    nodes.reduce(fn(acc, node) { 
      if node.voted_for == Some(candidate_id) { acc + 1 } else { acc } 
    }, 0)
  }
  
  // Declare leader if candidate has majority
  let declare_leader = fn(nodes: Array[NodeState>, candidate_id: String) {
    let votes = count_votes(nodes, candidate_id)
    let majority = (nodes.length() / 2) + 1
    
    if votes >= majority {
      let mut updated_nodes = []
      
      for node in nodes {
        updated_nodes = updated_nodes.push({
          id: node.id,
          term: node.term,
          voted_for: node.voted_for,
          is_leader: node.id == candidate_id,
          log: node.log
        })
      }
      
      updated_nodes
    } else {
      nodes
    }
  }
  
  // Test leader election
  let election_nodes = start_election(consensus_nodes, "node-0")
  assert_eq(count_votes(election_nodes, "node-0"), 1)  // Candidate voted for itself
  
  // Simulate other nodes voting
  let mut updated_nodes = election_nodes
  for i in 1..3 {
    updated_nodes[i] = { updated_nodes[i] | voted_for: Some("node-0") }
  }
  
  // Now node-0 should have 4 votes (itself + 3 others), which is a majority
  assert_eq(count_votes(updated_nodes, "node-0"), 4)
  
  // Declare leader
  let leader_nodes = declare_leader(updated_nodes, "node-0")
  
  // Verify leader election
  let leader = leader_nodes.find(fn(n) { n.is_leader })
  match leader {
    Some(node) => assert_eq(node.id, "node-0")
    None => assert_true(false)
  }
  
  // Verify only one leader
  let leaders = leader_nodes.filter(fn(n) { n.is_leader })
  assert_eq(leaders.length(), 1)
  
  // Test log replication
  let replicate_log = fn(nodes: Array[NodeState], leader_id: String, entry: (Int, String)) {
    let mut updated_nodes = []
    
    for node in nodes {
      if node.id == leader_id {
        // Leader appends to its own log
        updated_nodes = updated_nodes.push({
          id: node.id,
          term: node.term,
          voted_for: node.voted_for,
          is_leader: node.is_leader,
          log: node.log.push(entry)
        })
      } else {
        // Followers replicate the log entry
        updated_nodes = updated_nodes.push({
          id: node.id,
          term: node.term,
          voted_for: node.voted_for,
          is_leader: node.is_leader,
          log: node.log.push(entry)
        })
      }
    }
    
    updated_nodes
  }
  
  // Replicate a log entry
  let replicated_nodes = replicate_log(leader_nodes, "node-0", (1, "test-command"))
  
  // Verify all nodes have the same log
  for node in replicated_nodes {
    assert_eq(node.log.length(), 1)
    assert_eq(node.log[0], (1, "test-command"))
  }
  
  // Test leader failure and re-election
  let simulate_leader_failure = fn(nodes: Array[NodeState], leader_id: String) {
    let mut updated_nodes = []
    
    for node in nodes {
      if node.id == leader_id {
        // Leader fails (steps down)
        updated_nodes = updated_nodes.push({
          id: node.id,
          term: node.term + 1,
          voted_for: None,
          is_leader: false,
          log: node.log
        })
      } else {
        // Other nodes notice leader failure
        updated_nodes = updated_nodes.push({
          id: node.id,
          term: node.term,
          voted_for: None,
          is_leader: false,
          log: node.log
        })
      }
    }
    
    updated_nodes
  }
  
  let failure_nodes = simulate_leader_failure(replicated_nodes, "node-0")
  
  // Verify no leader
  let leaders_after_failure = failure_nodes.filter(fn(n) { n.is_leader })
  assert_eq(leaders_after_failure.length(), 0)
  
  // Start new election with different candidate
  let new_election_nodes = start_election(failure_nodes, "node-1")
  
  // Simulate votes for new candidate
  let mut new_updated_nodes = new_election_nodes
  for i in 0..5 {
    if i != 1 {  // All except the candidate itself
      new_updated_nodes[i] = { new_updated_nodes[i] | voted_for: Some("node-1") }
    }
  }
  
  // Declare new leader
  let new_leader_nodes = declare_leader(new_updated_nodes, "node-1")
  
  // Verify new leader
  let new_leader = new_leader_nodes.find(fn(n) { n.is_leader })
  match new_leader {
    Some(node) => assert_eq(node.id, "node-1")
    None => assert_true(false)
  }
  
  // Test log consistency after leader change
  let consistent_log_entry = (2, "consistency-test")
  let consistent_nodes = replicate_log(new_leader_nodes, "node-1", consistent_log_entry)
  
  // Verify all nodes have consistent logs
  for node in consistent_nodes {
    assert_eq(node.log.length(), 2)
    assert_eq(node.log[0], (1, "test-command"))
    assert_eq(node.log[1], (2, "consistency-test"))
  }
  
  // Test consensus validation
  let validate_consensus = fn(nodes: Array[NodeState]) {
    let mut leader_count = 0
    let mut leader_id = None
    
    for node in nodes {
      if node.is_leader {
        leader_count = leader_count + 1
        leader_id = Some(node.id)
      }
    }
    
    // Should have exactly one leader
    if leader_count != 1 {
      return false
    }
    
    // All nodes should have the same log
    let leader_log = match leader_id {
      Some(id) => match nodes.find(fn(n) { n.id == id }) {
        Some(node) => node.log,
        None => []
      }
      None => []
    }
    
    for node in nodes {
      if node.log.length() != leader_log.length() {
        return false
      }
      
      for i in 0..node.log.length() {
        if node.log[i] != leader_log[i] {
          return false
        }
      }
    }
    
    true
  }
  
  // Verify consensus is maintained
  assert_true(validate_consensus(consistent_nodes))
}

// Test 4: Distributed Transaction Coordination
test "distributed transaction coordination" {
  // Transaction and participant structures
  type Transaction = {
    id: String,
    status: String,
    participants: Array[String],
    operations: Array[(String, String, String)]  // (participant, key, value)
  }
  
  type ParticipantState = {
    id: String,
    data: Array[(String, String)],
    transactions: Array[String],  // Transaction IDs
    prepared: Array[String>,       // Prepared transactions
    committed: Array<String]       // Committed transactions
  }
  
  // Create initial participants
  let create_participants = fn(count: Int) {
    let mut participants = []
    
    for i in 0..count {
      participants = participants.push({
        id: "participant-" + i.to_string(),
        data: [],
        transactions: [],
        prepared: [],
        committed: []
      })
    }
    
    participants
  }
  
  let participants = create_participants(3)
  assert_eq(participants.length(), 3)
  
  // Create transaction
  let create_transaction = fn(id: String, participants: Array[String>, operations: Array[(String, String, String)]) {
    {
      id,
      status: "pending",
      participants,
      operations
    }
  }
  
  // Two-phase commit implementation
  let prepare_transaction = fn(transaction: Transaction, participants: Array[ParticipantState]) {
    let mut updated_participants = []
    let mut all_prepared = true
    
    for participant in participants {
      if transaction.participants.contains(participant.id) {
        // Check if participant can prepare all its operations
        let mut can_prepare = true
        
        for operation in transaction.operations {
          if operation.0 == participant.id {
            // Check if key exists (simplified validation)
            can_prepare = true  // Assume always can prepare for this test
          }
        }
        
        if can_prepare {
          // Add to prepared transactions
          updated_participants = updated_participants.push({
            id: participant.id,
            data: participant.data,
            transactions: participant.transactions.push(transaction.id),
            prepared: participant.prepared.push(transaction.id),
            committed: participant.committed
          })
        } else {
          // Cannot prepare, abort
          all_prepared = false
          updated_participants = updated_participants.push({
            id: participant.id,
            data: participant.data,
            transactions: participant.transactions,
            prepared: participant.prepared,
            committed: participant.committed
          })
        }
      } else {
        updated_participants = updated_participants.push(participant)
      }
    }
    
    (updated_participants, all_prepared)
  }
  
  let commit_transaction = fn(transaction: Transaction, participants: Array[ParticipantState>) {
    let mut updated_participants = []
    
    for participant in participants {
      if transaction.participants.contains(participant.id) {
        // Apply operations and mark as committed
        let mut updated_data = participant.data
        
        for operation in transaction.operations {
          if operation.0 == participant.id {
            let key = operation.1
            let value = operation.2
            
            // Check if key already exists
            let mut found = false
            let mut new_data = []
            
            for item in updated_data {
              if item.0 == key {
                new_data = new_data.push((key, value))
                found = true
              } else {
                new_data = new_data.push(item)
              }
            }
            
            if not(found) {
              new_data = new_data.push((key, value))
            }
            
            updated_data = new_data
          }
        }
        
        updated_participants = updated_participants.push({
          id: participant.id,
          data: updated_data,
          transactions: participant.transactions,
          prepared: participant.prepared.filter(fn(t) { t != transaction.id }),
          committed: participant.committed.push(transaction.id)
        })
      } else {
        updated_participants = updated_participants.push(participant)
      }
    }
    
    updated_participants
  }
  
  let abort_transaction = fn(transaction: Transaction, participants: Array[ParticipantState>) {
    let mut updated_participants = []
    
    for participant in participants {
      if transaction.participants.contains(participant.id) {
        // Remove from prepared transactions
        updated_participants = updated_participants.push({
          id: participant.id,
          data: participant.data,
          transactions: participant.transactions.filter(fn(t) { t != transaction.id }),
          prepared: participant.prepared.filter(fn(t) { t != transaction.id }),
          committed: participant.committed
        })
      } else {
        updated_participants = updated_participants.push(participant)
      }
    }
    
    updated_participants
  }
  
  // Create a transaction
  let transaction = create_transaction(
    "tx-123",
    ["participant-0", "participant-1", "participant-2"],
    [
      ("participant-0", "key-1", "value-1"),
      ("participant-1", "key-2", "value-2"),
      ("participant-2", "key-3", "value-3")
    ]
  )
  
  assert_eq(transaction.id, "tx-123")
  assert_eq(transaction.status, "pending")
  assert_eq(transaction.participants.length(), 3)
  assert_eq(transaction.operations.length(), 3)
  
  // Phase 1: Prepare
  let (prepared_participants, all_prepared) = prepare_transaction(transaction, participants)
  assert_true(all_prepared)
  
  // Verify all participants have prepared the transaction
  for participant in prepared_participants {
    if transaction.participants.contains(participant.id) {
      assert_true(participant.prepared.contains(transaction.id))
      assert_true(participant.transactions.contains(transaction.id))
    }
  }
  
  // Phase 2: Commit
  let committed_participants = commit_transaction(transaction, prepared_participants)
  
  // Verify all participants have committed the transaction
  for participant in committed_participants {
    if transaction.participants.contains(participant.id) {
      assert_true(participant.committed.contains(transaction.id))
      assert_false(participant.prepared.contains(transaction.id))  // Should be removed from prepared
      
      // Verify data was updated
      let expected_operation = transaction.operations.find(fn(op) { op.0 == participant.id })
      match expected_operation {
        Some(operation) => {
          assert_true(participant.data.contains((operation.1, operation.2)))
        }
        None => assert_true(false)
      }
    }
  }
  
  // Test transaction abort
  let abort_transaction_id = "tx-456"
  let abort_tx = create_transaction(
    abort_transaction_id,
    ["participant-0", "participant-1"],
    [
      ("participant-0", "key-4", "value-4"),
      ("participant-1", "key-5", "value-5")
    ]
  )
  
  // Simulate one participant failing to prepare
  let mut abort_participants = committed_participants
  abort_participants[1] = { abort_participants[1] | data: abort_participants[1].data }  // Just to create a new object
  
  // Prepare phase (simulating failure)
  let (abort_prepared_participants, abort_all_prepared) = prepare_transaction(abort_tx, abort_participants)
  
  // For this test, we'll manually set one participant to not prepare
  abort_prepared_participants[0] = { abort_prepared_participants[0] | prepared: [] }
  
  // Since not all prepared, abort
  let aborted_participants = abort_transaction(abort_tx, abort_prepared_participants)
  
  // Verify transaction was aborted
  for participant in aborted_participants {
    assert_false(participant.committed.contains(abort_transaction_id))
    assert_false(participant.prepared.contains(abort_transaction_id))
    assert_false(participant.transactions.contains(abort_transaction_id))
    
    // Verify no data was updated for this transaction
    for operation in abort_tx.operations {
      if operation.0 == participant.id {
        assert_false(participant.data.contains((operation.1, operation.2)))
      }
    }
  }
  
  // Test concurrent transactions
  let concurrent_tx1 = create_transaction(
    "tx-concurrent-1",
    ["participant-0", "participant-1"],
    [
      ("participant-0", "concurrent-key-1", "concurrent-value-1"),
      ("participant-1", "concurrent-key-2", "concurrent-value-2")
    ]
  )
  
  let concurrent_tx2 = create_transaction(
    "tx-concurrent-2",
    ["participant-1", "participant-2"],
    [
      ("participant-1", "concurrent-key-3", "concurrent-value-3"),
      ("participant-2", "concurrent-key-4", "concurrent-value-4")
    ]
  )
  
  // Prepare first transaction
  let (concurrent1_prepared, _) = prepare_transaction(concurrent_tx1, aborted_participants)
  
  // Prepare second transaction
  let (concurrent2_prepared, _) = prepare_transaction(concurrent_tx2, concurrent1_prepared);
  
  // Commit both transactions
  let concurrent1_committed = commit_transaction(concurrent_tx1, concurrent2_prepared)
  let concurrent2_committed = commit_transaction(concurrent_tx2, concurrent1_committed)
  
  // Verify both transactions committed successfully
  for participant in concurrent2_committed {
    assert_true(participant.committed.contains("tx-concurrent-1") || 
               not(concurrent_tx1.participants.contains(participant.id)))
    
    assert_true(participant.committed.contains("tx-concurrent-2") || 
               not(concurrent_tx2.participants.contains(participant.id)))
  }
  
  // Test transaction isolation
  let verify_isolation = fn(participants: Array[ParticipantState]) {
    for participant in participants {
      for tx_id in participant.committed {
        // Verify that all operations for this transaction are present
        // This is a simplified check - in a real system, we'd verify atomicity
        assert_true(tx_id.length() > 0)
      }
    }
  }
  
  verify_isolation(concurrent2_committed)
}

// Test 5: Distributed Lock Service
test "distributed lock service" {
  // Lock and node structures
  type Lock = {
    resource: String,
    owner: String,
    expiration: Int,
    created_at: Int
  }
  
  type LockNode = {
    id: String,
    locks: Array[Lock],
    timestamp: Int
  }
  
  type LockRequest = {
    resource: String,
    requester: String,
    ttl: Int
  }
  
  // Create lock nodes
  let create_lock_nodes = fn(count: Int) {
    let mut nodes = []
    
    for i in 0..count {
      nodes = nodes.push({
        id: "node-" + i.to_string(),
        locks: [],
        timestamp: 1640995200
      })
    }
    
    nodes
  }
  
  let lock_nodes = create_lock_nodes(3)
  assert_eq(lock_nodes.length(), 3)
  
  // Acquire lock
  let acquire_lock = fn(nodes: Array[LockNode], request: LockRequest, current_time: Int) {
    let mut updated_nodes = []
    let mut lock_acquired = true
    
    // Check if lock is already held
    for node in nodes {
      for lock in node.locks {
        if lock.resource == request.resource {
          // Check if lock has expired
          if lock.expiration > current_time {
            lock_acquired = false
            break
          }
        }
      }
      
      if not(lock_acquired) {
        break
      }
    }
    
    if lock_acquired {
      // Grant the lock
      for node in nodes {
        let new_lock = {
          resource: request.resource,
          owner: request.requester,
          expiration: current_time + request.ttl,
          created_at: current_time
        }
        
        updated_nodes = updated_nodes.push({
          id: node.id,
          locks: node.locks.push(new_lock),
          timestamp: current_time
        })
      }
    } else {
      // Lock not acquired
      updated_nodes = nodes
    }
    
    (updated_nodes, lock_acquired)
  }
  
  // Release lock
  let release_lock = fn(nodes: Array[LockNode], resource: String, owner: String) {
    let mut updated_nodes = []
    
    for node in nodes {
      let mut filtered_locks = []
      
      for lock in node.locks {
        if not(lock.resource == resource && lock.owner == owner) {
          filtered_locks = filtered_locks.push(lock)
        }
      }
      
      updated_nodes = updated_nodes.push({
        id: node.id,
        locks: filtered_locks,
        timestamp: node.timestamp
      })
    }
    
    updated_nodes
  }
  
  // Check lock ownership
  let check_lock_ownership = fn(nodes: Array[LockNode], resource: String, owner: String, current_time: Int) {
    let mut is_owner = false
    let mut lock_exists = false
    
    for node in nodes {
      for lock in node.locks {
        if lock.resource == resource {
          lock_exists = true
          
          if lock.owner == owner && lock.expiration > current_time {
            is_owner = true
          }
        }
      }
    }
    
    (lock_exists, is_owner)
  }
  
  // Test lock acquisition
  let lock_request = {
    resource: "test-resource",
    requester: "client-1",
    ttl: 3600  // 1 hour
  }
  
  let (locked_nodes, lock_acquired) = acquire_lock(lock_nodes, lock_request, 1640995200)
  assert_true(lock_acquired)
  
  // Verify lock exists on all nodes
  for node in locked_nodes {
    let mut found_lock = false
    
    for lock in node.locks {
      if lock.resource == "test-resource" && lock.owner == "client-1" {
        found_lock = true
        assert_eq(lock.expiration, 1640995200 + 3600)
        assert_eq(lock.created_at, 1640995200)
        break
      }
    }
    
    assert_true(found_lock)
  }
  
  // Test lock ownership check
  let (lock_exists, is_owner) = check_lock_ownership(locked_nodes, "test-resource", "client-1", 1640995200)
  assert_true(lock_exists)
  assert_true(is_owner)
  
  // Test non-owner check
  let (_, non_owner) = check_lock_ownership(locked_nodes, "test-resource", "client-2", 1640995200)
  assert_false(non_owner)
  
  // Test lock acquisition when already locked
  let conflicting_request = {
    resource: "test-resource",
    requester: "client-2",
    ttl: 1800  // 30 minutes
  }
  
  let (conflict_nodes, conflict_acquired) = acquire_lock(locked_nodes, conflicting_request, 1640995200)
  assert_false(conflict_acquired)
  
  // Test lock release
  let released_nodes = release_lock(locked_nodes, "test-resource", "client-1")
  
  // Verify lock is released from all nodes
  for node in released_nodes {
    for lock in node.locks {
      assert_not_eq(lock.resource, "test-resource")
    }
  }
  
  // Test lock acquisition after release
  let (relocked_nodes, relock_acquired) = acquire_lock(released_nodes, conflicting_request, 1640995200)
  assert_true(relock_acquired)
  
  // Verify new lock exists
  let (new_lock_exists, new_is_owner) = check_lock_ownership(relocked_nodes, "test-resource", "client-2", 1640995200)
  assert_true(new_lock_exists)
  assert_true(new_is_owner)
  
  // Test lock expiration
  let expired_time = 1640995200 + 7200  // 2 hours later (after TTL)
  let (expired_lock_exists, expired_is_owner) = check_lock_ownership(relocked_nodes, "test-resource", "client-2", expired_time)
  assert_false(expired_lock_exists)
  assert_false(expired_is_owner)
  
  // Test acquiring expired lock
  let expired_request = {
    resource: "test-resource",
    requester: "client-3",
    ttl: 1800  // 30 minutes
  }
  
  let (expired_nodes, expired_acquired) = acquire_lock(relocked_nodes, expired_request, expired_time)
  assert_true(expired_acquired)
  
  // Test concurrent lock requests
  let concurrent_request1 = {
    resource: "concurrent-resource",
    requester: "client-1",
    ttl: 3600
  }
  
  let concurrent_request2 = {
    resource: "concurrent-resource",
    requester: "client-2",
    ttl: 3600
  }
  
  // First client acquires lock
  let (concurrent_nodes1, concurrent_acquired1) = acquire_lock(expired_nodes, concurrent_request1, 1640995200);
  assert_true(concurrent_acquired1)
  
  // Second client tries to acquire same lock
  let (_, concurrent_acquired2) = acquire_lock(concurrent_nodes1, concurrent_request2, 1640995200)
  assert_false(concurrent_acquired2)
  
  // Test lock renewal
  let renew_lock = fn(nodes: Array[LockNode], resource: String, owner: String, ttl: Int, current_time: Int) {
    let mut updated_nodes = []
    let mut renewed = false
    
    for node in nodes {
      let mut updated_locks = []
      
      for lock in node.locks {
        if lock.resource == resource && lock.owner == owner && lock.expiration > current_time {
          // Renew the lock
          updated_locks = updated_locks.push({
            resource: lock.resource,
            owner: lock.owner,
            expiration: current_time + ttl,
            created_at: lock.created_at
          })
          renewed = true
        } else {
          updated_locks = updated_locks.push(lock)
        }
      }
      
      updated_nodes = updated_nodes.push({
        id: node.id,
        locks: updated_locks,
        timestamp: current_time
      })
    }
    
    (updated_nodes, renewed)
  }
  
  // Renew existing lock
  let (renewed_nodes, lock_renewed) = renew_lock(
    concurrent_nodes1, 
    "concurrent-resource", 
    "client-1", 
    7200,  // 2 hours
    1640995800  // 10 minutes later
  )
  
  assert_true(lock_renewed)
  
  // Verify lock was renewed
  let (renewed_lock_exists, renewed_is_owner) = check_lock_ownership(
    renewed_nodes, 
    "concurrent-resource", 
    "client-1", 
    1640995800
  )
  
  assert_true(renewed_lock_exists)
  assert_true(renewed_is_owner)
  
  // Test lock transfer
  let transfer_lock = fn(nodes: Array[LockNode], resource: String, current_owner: String, new_owner: String, ttl: Int, current_time: Int) {
    // First release the lock from current owner
    let released_nodes = release_lock(nodes, resource, current_owner)
    
    // Then acquire lock for new owner
    let transfer_request = {
      resource,
      requester: new_owner,
      ttl
    }
    
    acquire_lock(released_nodes, transfer_request, current_time)
  }
  
  // Transfer lock
  let (transferred_nodes, transfer_acquired) = transfer_lock(
    renewed_nodes,
    "concurrent-resource",
    "client-1",
    "client-2",
    3600,
    1640996000  // 20 minutes later
  )
  
  assert_true(transfer_acquired)
  
  // Verify lock was transferred
  let (transferred_lock_exists, transferred_is_owner) = check_lock_ownership(
    transferred_nodes,
    "concurrent-resource",
    "client-2",
    1640996000
  )
  
  assert_true(transferred_lock_exists)
  assert_true(transferred_is_owner)
  
  let (transferred_old_owner, _) = check_lock_ownership(
    transferred_nodes,
    "concurrent-resource",
    "client-1",
    1640996000
  )
  
  assert_false(transferred_old_owner)
}

// Test 6: Gossip Protocol for Data Dissemination
test "gossip protocol for data dissemination" {
  // Node and message structures
  type GossipNode = {
    id: String,
    data: Array[(String, String)],
    seen_messages: Array[String>,
    timestamp: Int
  }
  
  type GossipMessage = {
    id: String,
    origin: String,
    payload: Array[(String, String)],
    timestamp: Int,
    ttl: Int
  }
  
  // Create gossip nodes
  let create_gossip_nodes = fn(count: Int) {
    let mut nodes = []
    
    for i in 0..count {
      nodes = nodes.push({
        id: "node-" + i.to_string(),
        data: [],
        seen_messages: [],
        timestamp: 1640995200
      })
    }
    
    nodes
  }
  
  let gossip_nodes = create_gossip_nodes(5)
  assert_eq(gossip_nodes.length(), 5)
  
  // Create gossip message
  let create_gossip_message = fn(id: String, origin: String, payload: Array[(String, String)], timestamp: Int, ttl: Int) {
    {
      id,
      origin,
      payload,
      timestamp,
      ttl
    }
  }
  
  // Gossip dissemination
  let gossip_disseminate = fn(nodes: Array[GossipNode], message: GossipMessage, fanout: Int) {
    let mut updated_nodes = []
    let mut message_nodes = []
    
    // Find nodes that haven't seen this message
    for node in nodes {
      if not(node.seen_messages.contains(message.id)) {
        message_nodes = message_nodes.push(node)
      } else {
        updated_nodes = updated_nodes.push(node)
      }
    }
    
    // Select random fanout nodes to receive the message
    let fanout_nodes = if message_nodes.length() <= fanout {
      message_nodes
    } else {
      message_nodes.slice(0, fanout)
    }
    
    // Update selected nodes with the message
    for node in nodes {
      let mut selected = false
      
      for fanout_node in fanout_nodes {
        if node.id == fanout_node.id {
          selected = true
          break
        }
      }
      
      if selected {
        // Apply message payload to node data
        let mut updated_data = node.data
        
        for item in message.payload {
          let mut found = false
          let mut new_data = []
          
          for existing in updated_data {
            if existing.0 == item.0 {
              new_data = new_data.push(item)
              found = true
            } else {
              new_data = new_data.push(existing)
            }
          }
          
          if not(found) {
            new_data = new_data.push(item)
          }
          
          updated_data = new_data
        }
        
        updated_nodes = updated_nodes.push({
          id: node.id,
          data: updated_data,
          seen_messages: node.seen_messages.push(message.id),
          timestamp: node.timestamp
        })
      } else {
        updated_nodes = updated_nodes.push(node)
      }
    }
    
    updated_nodes
  }
  
  // Create initial message
  let initial_message = create_gossip_message(
    "msg-123",
    "node-0",
    [("key-1", "value-1"), ("key-2", "value-2")],
    1640995200,
    3  // TTL of 3 hops
  )
  
  // First dissemination from origin node
  let first_dissemination = gossip_disseminate(gossip_nodes, initial_message, 2)
  
  // Verify message was disseminated to fanout nodes
  let mut message_count = 0
  for node in first_dissemination {
    if node.seen_messages.contains("msg-123") {
      message_count = message_count + 1
      
      // Verify payload was applied
      assert_true(node.data.contains(("key-1", "value-1")))
      assert_true(node.data.contains(("key-2", "value-2")))
    }
  }
  
  assert_eq(message_count, 2)  // Fanout of 2
  
  // Second dissemination
  let ttl_decremented = {
    id: initial_message.id,
    origin: initial_message.origin,
    payload: initial_message.payload,
    timestamp: initial_message.timestamp,
    ttl: initial_message.ttl - 1
  }
  
  let second_dissemination = gossip_disseminate(first_dissemination, ttl_decremented, 2)
  
  // Count nodes that have seen the message
  message_count = 0
  for node in second_dissemination {
    if node.seen_messages.contains("msg-123") {
      message_count = message_count + 1
    }
  }
  
  assert_true(message_count >= 2)  // At least the previous nodes
  
  // Third dissemination (final hop)
  let ttl_final = {
    id: initial_message.id,
    origin: initial_message.origin,
    payload: initial_message.payload,
    timestamp: initial_message.timestamp,
    ttl: ttl_decremented.ttl - 1
  }
  
  let final_dissemination = gossip_disseminate(second_dissemination, ttl_final, 2)
  
  // Count nodes that have seen the message
  message_count = 0
  for node in final_dissemination {
    if node.seen_messages.contains("msg-123") {
      message_count = message_count + 1
    }
  }
  
  assert_true(message_count >= 2)  // At least some nodes
  
  // Test anti-entropy (nodes exchanging data to ensure consistency)
  let anti_entropy_exchange = fn(nodes: Array[GossipNode]) {
    let mut updated_nodes = []
    
    // Collect all unique data items from all nodes
    let mut all_data = {}
    
    for node in nodes {
      for item in node.data {
        all_data = all_data.insert(item.0, item.1)
      }
    }
    
    // Update each node with all data
    for node in nodes {
      let mut updated_data = []
      
      for (key, value) in all_data {
        updated_data = updated_data.push((key, value))
      }
      
      updated_nodes = updated_nodes.push({
        id: node.id,
        data: updated_data,
        seen_messages: node.seen_messages,
        timestamp: node.timestamp
      })
    }
    
    updated_nodes
  }
  
  // Add some divergent data to nodes
  let mut divergent_nodes = []
  for i in 0..final_dissemination.length() {
    let node = final_dissemination[i]
    let node_specific_data = ("node-specific-" + i.to_string(), "value-" + i.to_string())
    
    divergent_nodes = divergent_nodes.push({
      id: node.id,
      data: node.data.push(node_specific_data),
      seen_messages: node.seen_messages,
      timestamp: node.timestamp
    })
  }
  
  // Perform anti-entropy exchange
  let consistent_nodes = anti_entropy_exchange(divergent_nodes)
  
  // Verify all nodes now have all data
  for node in consistent_nodes {
    // Should have original message data
    assert_true(node.data.contains(("key-1", "value-1")))
    assert_true(node.data.contains(("key-2", "value-2")))
    
    // Should have all node-specific data
    for i in 0..consistent_nodes.length() {
      assert_true(node.data.contains(("node-specific-" + i.to_string(), "value-" + i.to_string())))
    }
  }
  
  // Test message expiration
  let expired_message = create_gossip_message(
    "msg-expired",
    "node-0",
    [("expired-key", "expired-value")],
    1640995200,
    0  // TTL of 0
  )
  
  let expired_dissemination = gossip_disseminate(consistent_nodes, expired_message, 2)
  
  // Message with TTL 0 should not be disseminated
  for node in expired_dissemination {
    assert_false(node.seen_messages.contains("msg-expired"))
    assert_false(node.data.contains(("expired-key", "expired-value")))
  }
  
  // Test message deduplication
  let duplicate_message = create_gossip_message(
    "msg-123",  // Same ID as initial message
    "node-1",   // Different origin
    [("key-3", "value-3")],  // Different payload
    1640995300,
    2
  )
  
  let duplicate_dissemination = gossip_disseminate(expired_dissemination, duplicate_message, 2)
  
  // Nodes should ignore duplicate messages
  for node in duplicate_dissemination {
    if node.seen_messages.contains("msg-123") {
      // Should not have new payload
      assert_false(node.data.contains(("key-3", "value-3")))
    }
  }
  
  // Test convergence metrics
  let calculate_convergence = fn(nodes: Array[GossipNode>) {
    let mut convergence_metrics = {}
    
    // Calculate data similarity between all pairs of nodes
    for i in 0..nodes.length() {
      for j in i..nodes.length() {
        let node1 = nodes[i]
        let node2 = nodes[j]
        
        let mut common_items = 0
        let mut total_items = 0
        
        // Count common items
        for item1 in node1.data {
          total_items = total_items + 1
          
          for item2 in node2.data {
            if item1 == item2 {
              common_items = common_items + 1
              break
            }
          }
        }
        
        // Add items only in node2
        for item2 in node2.data {
          let mut found = false
          
          for item1 in node1.data {
            if item1 == item2 {
              found = true
              break
            }
          }
          
          if not(found) {
            total_items = total_items + 1
          }
        }
        
        let similarity = if total_items > 0 {
          common_items.to_float() / total_items.to_float()
        } else {
          1.0
        }
        
        convergence_metrics = convergence_metrics.insert((i, j), similarity)
      }
    }
    
    convergence_metrics
  }
  
  let convergence_metrics = calculate_convergence(duplicate_dissemination)
  
  // All node pairs should have high similarity
  for ((i, j), similarity) in convergence_metrics {
    assert_true(similarity > 0.8)  // At least 80% similar
  }
}

// Test 7: Consistent Hashing for Data Distribution
test "consistent hashing for data distribution" {
  // Hash ring and node structures
  type HashRing = {
    nodes: Array[(Int, String)>,  // (hash, node_id)
    virtual_nodes: Int
  }
  
  type DataDistribution = {
    key: String,
    node_id: String,
    hash: Int
  }
  
  // Simple hash function (simulated)
  let simple_hash = fn(key: String) {
    let mut hash = 0
    
    for i in 0..key.length() {
      hash = (hash * 31 + key[i].to_int()) % 1000000  // Mod to keep it small
    }
    
    if hash < 0 {
      hash = hash + 1000000
    }
    
    hash
  }
  
  // Create consistent hash ring
  let create_hash_ring = fn(node_ids: Array[String], virtual_nodes: Int) {
    let mut ring_nodes = []
    
    for node_id in node_ids {
      for i in 0..virtual_nodes {
        let virtual_node_id = node_id + "-virtual-" + i.to_string()
        let hash = simple_hash(virtual_node_id)
        ring_nodes = ring_nodes.push((hash, node_id))
      }
    }
    
    // Sort by hash
    ring_nodes.sort(fn(a, b) { if a.0 < b.0 { -1 } else if a.0 > b.0 { 1 } else { 0 } })
    
    {
      nodes: ring_nodes,
      virtual_nodes
    }
  }
  
  // Test hash ring creation
  let node_ids = ["node-1", "node-2", "node-3"]
  let hash_ring = create_hash_ring(node_ids, 3)
  
  assert_eq(hash_ring.nodes.length(), 9)  // 3 nodes * 3 virtual nodes each
  assert_eq(hash_ring.virtual_nodes, 3)
  
  // Verify ring is sorted by hash
  for i in 1..hash_ring.nodes.length() {
    assert_true(hash_ring.nodes[i].0 >= hash_ring.nodes[i-1].0)
  }
  
  // Find node for a key
  let find_node = fn(ring: HashRing, key: String) {
    let key_hash = simple_hash(key)
    
    // Find the first node with hash >= key hash
    for (hash, node_id) in ring.nodes {
      if hash >= key_hash {
        return node_id
      }
    }
    
    // If not found, wrap around to the first node
    ring.nodes[0].1
  }
  
  // Test node finding
  let node_for_key1 = find_node(hash_ring, "test-key-1")
  assert_true(node_ids.contains(node_for_key1))
  
  let node_for_key2 = find_node(hash_ring, "test-key-2")
  assert_true(node_ids.contains(node_for_key2))
  
  // Test data distribution
  let distribute_data = fn(ring: HashRing, keys: Array[String]) {
    let mut distribution = {}
    
    for key in keys {
      let node_id = find_node(ring, key)
      let key_hash = simple_hash(key)
      
      let current_distribution = match distribution[node_id] {
        Some(dist) => dist,
        None => []
      }
      
      distribution = distribution.insert(node_id, current_distribution.push({
        key,
        node_id,
        hash: key_hash
      }))
    }
    
    distribution
  }
  
  // Create test keys
  let test_keys = [
    "user-123", "user-456", "user-789",
    "session-abc", "session-def", "session-ghi",
    "cache-key-1", "cache-key-2", "cache-key-3",
    "metric-cpu", "metric-memory", "metric-disk"
  ]
  
  let data_distribution = distribute_data(hash_ring, test_keys)
  
  // Verify all keys are distributed
  let mut total_keys = 0
  for (node_id, keys) in data_distribution {
    total_keys = total_keys + keys.length()
    
    // Verify all keys for this node are assigned to this node
    for key_data in keys {
      assert_eq(key_data.node_id, node_id)
      assert_eq(find_node(hash_ring, key_data.key), node_id)
    }
  }
  
  assert_eq(total_keys, test_keys.length())
  
  // Test adding a node
  let add_node = fn(ring: HashRing, node_id: String) {
    let mut new_nodes = ring.nodes
    
    // Add virtual nodes for the new node
    for i in 0..ring.virtual_nodes {
      let virtual_node_id = node_id + "-virtual-" + i.to_string()
      let hash = simple_hash(virtual_node_id)
      new_nodes = new_nodes.push((hash, node_id))
    }
    
    // Sort by hash
    new_nodes.sort(fn(a, b) { if a.0 < b.0 { -1 } else if a.0 > b.0 { 1 } else { 0 } })
    
    {
      nodes: new_nodes,
      virtual_nodes: ring.virtual_nodes
    }
  }
  
  let expanded_ring = add_node(hash_ring, "node-4")
  assert_eq(expanded_ring.nodes.length(), 12)  // 4 nodes * 3 virtual nodes each
  
  // Test removing a node
  let remove_node = fn(ring: HashRing, node_id: String) {
    let mut new_nodes = []
    
    // Remove all virtual nodes for the specified node
    for (hash, id) in ring.nodes {
      if id != node_id {
        new_nodes = new_nodes.push((hash, id))
      }
    }
    
    {
      nodes: new_nodes,
      virtual_nodes: ring.virtual_nodes
    }
  }
  
  let reduced_ring = remove_node(expanded_ring, "node-2")
  assert_eq(reduced_ring.nodes.length(), 9)  // 3 nodes * 3 virtual nodes each
  
  // Test data redistribution after node changes
  let original_distribution = data_distribution
  let expanded_distribution = distribute_data(expanded_ring, test_keys)
  let reduced_distribution = distribute_data(reduced_ring, test_keys)
  
  // Calculate redistribution impact
  let calculate_redistribution_impact = fn(original: {}, updated: {}) {
    let mut moved_keys = 0
    
    for (node_id, keys) in updated {
      for key_data in keys {
        let key = key_data.key
        
        // Check if this key was assigned to a different node originally
        let mut original_node = None
        
        for (orig_node_id, orig_keys) in original {
          for orig_key_data in orig_keys {
            if orig_key_data.key == key {
              original_node = Some(orig_node_id)
              break
            }
          }
          
          match original_node {
            Some(_) => break
            None => {}
          }
        }
        
        match original_node {
          Some(orig_node) => {
            if orig_node != node_id {
              moved_keys = moved_keys + 1
            }
          }
          None => {}
        }
      }
    }
    
    moved_keys
  }
  
  let expansion_impact = calculate_redistribution_impact(original_distribution, expanded_distribution)
  let reduction_impact = calculate_redistribution_impact(original_distribution, reduced_distribution)
  
  // Adding a node should move some keys
  assert_true(expansion_impact > 0)
  
  // Removing a node should move some keys
  assert_true(reduction_impact > 0)
  
  // Test load balancing
  let calculate_load_balance = fn(distribution: {}) {
    let mut loads = []
    
    for (node_id, keys) in distribution {
      loads = loads.push(keys.length())
    }
    
    if loads.length() == 0 {
      (0.0, 0.0, 0.0)
    } else {
      let total = loads.reduce(fn(acc, load) { acc + load }, 0)
      let mean = total.to_float() / loads.length().to_float()
      
      let variance = loads.reduce(fn(acc, load) { 
        acc + (load.to_float() - mean) * (load.to_float() - mean)
      }, 0.0) / loads.length().to_float()
      
      let std_dev = variance.sqrt()
      
      (mean, std_dev, variance)
    }
  }
  
  let (original_mean, original_std_dev, _) = calculate_load_balance(original_distribution)
  let (expanded_mean, expanded_std_dev, _) = calculate_load_balance(expanded_distribution)
  let (reduced_mean, reduced_std_dev, _) = calculate_load_balance(reduced_distribution)
  
  // Mean should be approximately total_keys / number_of_nodes
  assert_eq(original_mean, test_keys.length().to_float() / 3.0)
  assert_eq(expanded_mean, test_keys.length().to_float() / 4.0)
  assert_eq(reduced_mean, test_keys.length().to_float() / 3.0)
  
  // Standard deviation should be relatively small (good load balancing)
  assert_true(original_std_dev < original_mean * 0.5)  // Less than 50% of mean
  assert_true(expanded_std_dev < expanded_mean * 0.5)
  assert_true(reduced_std_dev < reduced_mean * 0.5)
  
  // Test hash ring consistency
  let verify_ring_consistency = fn(ring: HashRing) {
    // Verify no duplicate hashes
    let mut seen_hashes = {}
    
    for (hash, _) in ring.nodes {
      assert_false(seen_hashes.contains_key(hash))
      seen_hashes = seen_hashes.insert(hash, true)
    }
    
    // Verify ring is sorted
    for i in 1..ring.nodes.length() {
      assert_true(ring.nodes[i].0 >= ring.nodes[i-1].0)
    }
    
    // Verify all nodes have the expected number of virtual nodes
    let mut node_counts = {}
    
    for (_, node_id) in ring.nodes {
      let count = match node_counts[node_id] {
        Some(c) => c + 1,
        None => 1
      }
      node_counts = node_counts.insert(node_id, count)
    }
    
    for (_, count) in node_counts {
      assert_eq(count, ring.virtual_nodes)
    }
  }
  
  verify_ring_consistency(hash_ring)
  verify_ring_consistency(expanded_ring)
  verify_ring_consistency(reduced_ring)
}

// Test 8: Distributed Cache Consistency
test "distributed cache consistency" {
  // Cache node and entry structures
  type CacheEntry = {
    key: String,
    value: String,
    version: Int,
    timestamp: Int,
    ttl: Int
  }
  
  type CacheNode = {
    id: String,
    cache: Array[CacheEntry],
    timestamp: Int
  }
  
  type CacheOperation = {
    operation: String,  // "put", "get", "delete", "invalidate"
    key: String,
    value: Option<String>,
    version: Option[Int>,
    source: String
  }
  
  // Create cache nodes
  let create_cache_nodes = fn(count: Int) {
    let mut nodes = []
    
    for i in 0..count {
      nodes = nodes.push({
        id: "cache-node-" + i.to_string(),
        cache: [],
        timestamp: 1640995200
      })
    }
    
    nodes
  }
  
  let cache_nodes = create_cache_nodes(3)
  assert_eq(cache_nodes.length(), 3)
  
  // Apply cache operation
  let apply_cache_operation = fn(node: CacheNode, operation: CacheOperation, current_time: Int) {
    match operation.operation {
      "put" => {
        match operation.value {
          Some(value) => {
            let version = match operation.version {
              Some(v) => v,
              None => 1
            }
            
            let ttl = 3600  // Default TTL of 1 hour
            
            // Check if key already exists
            let mut updated_cache = []
            let mut found = false
            
            for entry in node.cache {
              if entry.key == operation.key {
                updated_cache = updated_cache.push({
                  key: operation.key,
                  value,
                  version,
                  timestamp: current_time,
                  ttl
                })
                found = true
              } else {
                updated_cache = updated_cache.push(entry)
              }
            }
            
            if not(found) {
              updated_cache = updated_cache.push({
                key: operation.key,
                value,
                version,
                timestamp: current_time,
                ttl
              })
            }
            
            {
              id: node.id,
              cache: updated_cache,
              timestamp: current_time
            }
          }
          None => node
        }
      }
      "delete" => {
        let mut updated_cache = []
        
        for entry in node.cache {
          if entry.key != operation.key {
            updated_cache = updated_cache.push(entry)
          }
        }
        
        {
          id: node.id,
          cache: updated_cache,
          timestamp: current_time
        }
      }
      "invalidate" => {
        // Similar to delete but with version checking
        match operation.version {
          Some(version) => {
            let mut updated_cache = []
            
            for entry in node.cache {
              if entry.key != operation.key || entry.version < version {
                updated_cache = updated_cache.push(entry)
              }
            }
            
            {
              id: node.id,
              cache: updated_cache,
              timestamp: current_time
            }
          }
          None => node
        }
      }
      _ => node
    }
  }
  
  // Replicate operation to all nodes
  let replicate_operation = fn(nodes: Array[CacheNode], operation: CacheOperation, current_time: Int) {
    let mut updated_nodes = []
    
    for node in nodes {
      updated_nodes = updated_nodes.push(apply_cache_operation(node, operation, current_time))
    }
    
    updated_nodes
  }
  
  // Test cache put operation
  let put_operation = {
    operation: "put",
    key: "test-key",
    value: Some("test-value"),
    version: Some(1),
    source: "client-1"
  }
  
  let put_nodes = replicate_operation(cache_nodes, put_operation, 1640995200)
  
  // Verify all nodes have the entry
  for node in put_nodes {
    let mut found = false
    
    for entry in node.cache {
      if entry.key == "test-key" {
        assert_eq(entry.value, "test-value")
        assert_eq(entry.version, 1)
        assert_eq(entry.timestamp, 1640995200)
        assert_eq(entry.ttl, 3600)
        found = true
        break
      }
    }
    
    assert_true(found)
  }
  
  // Test cache get operation
  let get_from_cache = fn(node: CacheNode, key: String, current_time: Int) {
    for entry in node.cache {
      if entry.key == key {
        // Check if entry has expired
        if current_time - entry.timestamp < entry.ttl {
          return Some(entry.value)
        } else {
          return None  // Expired
        }
      }
    }
    
    None  // Not found
  }
  
  // Test get operation
  let get_result = get_from_cache(put_nodes[0], "test-key", 1640995300)
  assert_eq(get_result, Some("test-value"))
  
  // Test get of non-existent key
  let get_nonexistent = get_from_cache(put_nodes[0], "non-existent-key", 1640995300)
  assert_eq(get_nonexistent, None)
  
  // Test get of expired key
  let get_expired = get_from_cache(put_nodes[0], "test-key", 1640995200 + 4000)  // After TTL
  assert_eq(get_expired, None)
  
  // Test cache update operation
  let update_operation = {
    operation: "put",
    key: "test-key",
    value: Some("updated-value"),
    version: Some(2),
    source: "client-2"
  }
  
  let updated_nodes = replicate_operation(put_nodes, update_operation, 1640995400)
  
  // Verify all nodes have the updated entry
  for node in updated_nodes {
    let mut found = false
    
    for entry in node.cache {
      if entry.key == "test-key" {
        assert_eq(entry.value, "updated-value")
        assert_eq(entry.version, 2)
        assert_eq(entry.timestamp, 1640995400)
        found = true
        break
      }
    }
    
    assert_true(found)
  }
  
  // Test cache delete operation
  let delete_operation = {
    operation: "delete",
    key: "test-key",
    value: None,
    version: None,
    source: "client-1"
  }
  
  let deleted_nodes = replicate_operation(updated_nodes, delete_operation, 1640995500)
  
  // Verify entry is deleted from all nodes
  for node in deleted_nodes {
    let mut found = false
    
    for entry in node.cache {
      if entry.key == "test-key" {
        found = true
        break
      }
    }
    
    assert_false(found)
  }
  
  // Test cache invalidate operation
  let invalidate_operation = {
    operation: "put",
    key: "invalidate-key",
    value: Some("invalidate-value"),
    version: Some(1),
    source: "client-1"
  }
  
  let invalidate_nodes = replicate_operation(deleted_nodes, invalidate_operation, 1640995600)
  
  // Add another entry with higher version
  let invalidate_update_operation = {
    operation: "put",
    key: "invalidate-key",
    value: Some("invalidate-updated"),
    version: Some(2),
    source: "client-2"
  }
  
  let invalidate_updated_nodes = replicate_operation(invalidate_nodes, invalidate_update_operation, 1640995700)
  
  // Now invalidate with version 1 (should not affect version 2)
  let invalidate_version_operation = {
    operation: "invalidate",
    key: "invalidate-key",
    value: None,
    version: Some(1),
    source: "cache-admin"
  }
  
  let invalidate_version_nodes = replicate_operation(invalidate_updated_nodes, invalidate_version_operation, 1640995800)
  
  // Verify entry with version 2 still exists
  for node in invalidate_version_nodes {
    let mut found = false
    
    for entry in node.cache {
      if entry.key == "invalidate-key" {
        assert_eq(entry.value, "invalidate-updated")
        assert_eq(entry.version, 2)
        found = true
        break
      }
    }
    
    assert_true(found)
  }
  
  // Test cache consistency validation
  let validate_cache_consistency = fn(nodes: Array[CacheNode]) {
    let mut consistency_issues = []
    
    // Collect all keys from all nodes
    let mut all_keys = {}
    
    for node in nodes {
      for entry in node.cache {
        let key_entries = match all_keys[entry.key] {
          Some(entries) => entries,
          None => []
        }
        
        all_keys = all_keys.insert(entry.key, key_entries.push(entry))
      }
    }
    
    // Check consistency for each key
    for (key, entries) in all_keys {
      if entries.length() > 1 {
        // All entries for the same key should have the same value and version
        let first_entry = entries[0]
        
        for entry in entries {
          if entry.value != first_entry.value || entry.version != first_entry.version {
            consistency_issues = consistency_issues.push({
              key,
              issue: "Value or version mismatch",
              details: "Expected: " + first_entry.value + " v" + first_entry.version.to_string() + 
                      ", Found: " + entry.value + " v" + entry.version.to_string()
            })
          }
        }
      }
    }
    
    consistency_issues
  }
  
  // Create a scenario with inconsistency
  let mut inconsistent_nodes = []
  for i in 0..3 {
    let node = invalidate_version_nodes[i]
    
    if i == 0 {
      // Make this node inconsistent
      let mut inconsistent_cache = []
      
      for entry in node.cache {
        if entry.key == "invalidate-key" {
          inconsistent_cache = inconsistent_cache.push({
            key: entry.key,
            value: "inconsistent-value",
            version: entry.version,
            timestamp: entry.timestamp,
            ttl: entry.ttl
          })
        } else {
          inconsistent_cache = inconsistent_cache.push(entry)
        }
      }
      
      inconsistent_nodes = inconsistent_nodes.push({
        id: node.id,
        cache: inconsistent_cache,
        timestamp: node.timestamp
      })
    } else {
      inconsistent_nodes = inconsistent_nodes.push(node)
    }
  }
  
  // Validate consistency
  let consistency_issues = validate_cache_consistency(inconsistent_nodes)
  assert_eq(consistency_issues.length(), 1)
  assert_eq(consistency_issues[0].key, "invalidate-key")
  assert_eq(consistency_issues[0].issue, "Value or version mismatch")
  
  // Test cache repair mechanism
  let repair_cache_consistency = fn(nodes: Array[CacheNode]) {
    // Find majority value for each key
    let mut key_majorities = {}
    
    for node in nodes {
      for entry in node.cache {
        let key_values = match key_majorities[entry.key] {
          Some(values) => values,
          None => {}
        }
        
        let current_count = match key_values[(entry.value, entry.version)] {
          Some(count) => count + 1,
          None => 1
        }
        
        key_majorities = key_majorities.insert(entry.key, 
          key_values.insert((entry.value, entry.version), current_count))
      }
    }
    
    // Find majority for each key
    let mut majorities = {}
    
    for (key, values) in key_majorities {
      let mut majority = ("", 0, 0)  // (value, version, count)
      
      for ((value, version), count) in values {
        if count > majority.2 {
          majority = (value, version, count)
        }
      }
      
      majorities = majorities.insert(key, (majority.0, majority.1))
    }
    
    // Repair all nodes
    let mut repaired_nodes = []
    
    for node in nodes {
      let mut repaired_cache = []
      
      for entry in node.cache {
        match majorities[entry.key] {
          Some((majority_value, majority_version)) => {
            // Use majority value
            repaired_cache = repaired_cache.push({
              key: entry.key,
              value: majority_value,
              version: majority_version,
              timestamp: entry.timestamp,
              ttl: entry.ttl
            })
          }
          None => {
            repaired_cache = repaired_cache.push(entry)
          }
        }
      }
      
      repaired_nodes = repaired_nodes.push({
        id: node.id,
        cache: repaired_cache,
        timestamp: node.timestamp
      })
    }
    
    repaired_nodes
  }
  
  // Repair inconsistent cache
  let repaired_nodes = repair_cache_consistency(inconsistent_nodes)
  
  // Verify consistency is restored
  let repaired_consistency_issues = validate_cache_consistency(repaired_nodes)
  assert_eq(repaired_consistency_issues.length(), 0)
  
  // Verify all nodes now have the same value
  let expected_value = "invalidate-updated"
  let expected_version = 2
  
  for node in repaired_nodes {
    let mut found = false
    
    for entry in node.cache {
      if entry.key == "invalidate-key" {
        assert_eq(entry.value, expected_value)
        assert_eq(entry.version, expected_version)
        found = true
        break
      }
    }
    
    assert_true(found)
  }
}

// Test 9: Partition Tolerance and Network Partitions
test "partition tolerance and network partitions" {
  // Partition and node structures
  type Partition = {
    id: String,
    nodes: Array[String],
    is_majority: Bool
  }
  
  type PartitionedNode = {
    id: String,
    partition_id: String,
    data: Array[(String, String)],
    last_seen: Int
  }
  
  type PartitionedOperation = {
    operation: String,
    key: String,
    value: Option<String>,
    source_partition: String,
    timestamp: Int
  }
  
  // Create initial nodes
  let create_partitioned_nodes = fn(count: Int) {
    let mut nodes = []
    
    for i in 0..count {
      nodes = nodes.push({
        id: "node-" + i.to_string(),
        partition_id: "default",
        data: [],
        last_seen: 1640995200
      })
    }
    
    nodes
  }
  
  let partitioned_nodes = create_partitioned_nodes(5)
  assert_eq(partitioned_nodes.length(), 5)
  
  // Create network partitions
  let create_partitions = fn(nodes: Array[PartitionedNode], partition_config: Array[Array[String>>) {
    let mut partitions = []
    let mut updated_nodes = []
    
    for i in 0..partition_config.length() {
      let partition_nodes = partition_config[i]
      let is_majority = partition_nodes.length() > nodes.length() / 2
      
      partitions = partitions.push({
        id: "partition-" + i.to_string(),
        nodes: partition_nodes,
        is_majority
      })
    }
    
    // Update nodes with partition information
    for node in nodes {
      let mut partition_id = "unreachable"
      
      for i in 0..partition_config.length() {
        if partition_config[i].contains(node.id) {
          partition_id = "partition-" + i.to_string()
          break
        }
      }
      
      updated_nodes = updated_nodes.push({
        id: node.id,
        partition_id,
        data: node.data,
        last_seen: node.last_seen
      })
    }
    
    (partitions, updated_nodes)
  }
  
  // Create partitions: [node-0, node-1, node-2] and [node-3, node-4]
  let partition_config = [
    ["node-0", "node-1", "node-2"],
    ["node-3", "node-4"]
  ]
  
  let (partitions, partitioned_nodes_with_partitions) = create_partitions(partitioned_nodes, partition_config)
  
  assert_eq(partitions.length(), 2)
  assert_eq(partitions[0].id, "partition-0")
  assert_eq(partitions[0].nodes.length(), 3)
  assert_true(partitions[0].is_majority)
  
  assert_eq(partitions[1].id, "partition-1")
  assert_eq(partitions[1].nodes.length(), 2)
  assert_false(partitions[1].is_majority)
  
  // Verify nodes are assigned to correct partitions
  for node in partitioned_nodes_with_partitions {
    if node.id == "node-0" || node.id == "node-1" || node.id == "node-2" {
      assert_eq(node.partition_id, "partition-0")
    } else if node.id == "node-3" || node.id == "node-4" {
      assert_eq(node.partition_id, "partition-1")
    }
  }
  
  // Handle operations within partitions
  let handle_partitioned_operation = fn(nodes: Array[PartitionedNode], operation: PartitionedOperation) {
    let mut updated_nodes = []
    
    for node in nodes {
      if node.partition_id == operation.source_partition {
        // Apply operation only to nodes in the same partition
        let mut updated_data = node.data
        
        match operation.operation {
          "put" => {
            match operation.value {
              Some(value) => {
                let mut found = false
                let mut new_data = []
                
                for item in updated_data {
                  if item.0 == operation.key {
                    new_data = new_data.push((operation.key, value))
                    found = true
                  } else {
                    new_data = new_data.push(item)
                  }
                }
                
                if not(found) {
                  new_data = new_data.push((operation.key, value))
                }
                
                updated_data = new_data
              }
              None => {}
            }
          }
          "delete" => {
            let mut new_data = []
            
            for item in updated_data {
              if item.0 != operation.key {
                new_data = new_data.push(item)
              }
            }
            
            updated_data = new_data
          }
          _ => {}
        }
        
        updated_nodes = updated_nodes.push({
          id: node.id,
          partition_id: node.partition_id,
          data: updated_data,
          last_seen: operation.timestamp
        })
      } else {
        // Nodes in other partitions are not affected
        updated_nodes = updated_nodes.push(node)
      }
    }
    
    updated_nodes
  }
  
  // Test operations within majority partition
  let majority_operation = {
    operation: "put",
    key: "majority-key",
    value: Some("majority-value"),
    source_partition: "partition-0",
    timestamp: 1640995300
  }
  
  let nodes_after_majority_op = handle_partitioned_operation(partitioned_nodes_with_partitions, majority_operation)
  
  // Verify operation is applied only to nodes in majority partition
  for node in nodes_after_majority_op {
    if node.partition_id == "partition-0" {
      let mut found = false
      
      for item in node.data {
        if item.0 == "majority-key" && item.1 == "majority-value" {
          found = true
          break
        }
      }
      
      assert_true(found)
    } else {
      // Nodes in minority partition should not have this data
      let mut found = false
      
      for item in node.data {
        if item.0 == "majority-key" {
          found = true
          break
        }
      }
      
      assert_false(found)
    }
  }
  
  // Test operations within minority partition
  let minority_operation = {
    operation: "put",
    key: "minority-key",
    value: Some("minority-value"),
    source_partition: "partition-1",
    timestamp: 1640995400
  }
  
  let nodes_after_minority_op = handle_partitioned_operation(nodes_after_majority_op, minority_operation)
  
  // Verify operation is applied only to nodes in minority partition
  for node in nodes_after_minority_op {
    if node.partition_id == "partition-1" {
      let mut found = false
      
      for item in node.data {
        if item.0 == "minority-key" && item.1 == "minority-value" {
          found = true
          break
        }
      }
      
      assert_true(found)
    } else {
      // Nodes in majority partition should not have this data
      let mut found = false
      
      for item in node.data {
        if item.0 == "minority-key" {
          found = true
          break
        }
      }
      
      assert_false(found)
    }
  }
  
  // Test partition healing and conflict resolution
  let heal_partitions = fn(nodes: Array[PartitionedNode]) {
    let mut healed_nodes = []
    let mut all_data = {}
    
    // Collect all data from all nodes
    for node in nodes {
      for item in node.data {
        let key_versions = match all_data[item.0] {
          Some(versions) => versions,
          None => {}
        }
        
        all_data = all_data.insert(item.0, key_versions.insert(item.1, 1))
      }
    }
    
    // Update all nodes with all data
    for node in nodes {
      let mut updated_data = []
      
      for (key, versions) in all_data {
        for (value, _) in versions {
          updated_data = updated_data.push((key, value))
        }
      }
      
      healed_nodes = healed_nodes.push({
        id: node.id,
        partition_id: "healed",
        data: updated_data,
        last_seen: node.last_seen
      })
    }
    
    healed_nodes
  }
  
  // Heal partitions
  let healed_nodes = heal_partitions(nodes_after_minority_op)
  
  // Verify all nodes are in the healed partition
  for node in healed_nodes {
    assert_eq(node.partition_id, "healed")
  }
  
  // Verify all nodes have all data
  for node in healed_nodes {
    let mut majority_found = false
    let mut minority_found = false
    
    for item in node.data {
      if item.0 == "majority-key" && item.1 == "majority-value" {
        majority_found = true
      }
      
      if item.0 == "minority-key" && item.1 == "minority-value" {
        minority_found = true
      }
    }
    
    assert_true(majority_found)
    assert_true(minority_found)
  }
  
  // Test conflict resolution with timestamps
  let resolve_conflicts = fn(nodes: Array[PartitionedNode]) {
    let mut resolved_nodes = []
    let mut key_timestamps = {}
    
    // Collect latest timestamp for each key-value pair
    for node in nodes {
      for item in node.data {
        let current_timestamp = match key_timestamps[item.0] {
          Some(timestamp) => timestamp,
          None => node.last_seen
        }
        
        if node.last_seen > current_timestamp {
          key_timestamps = key_timestamps.insert(item.0, node.last_seen)
        }
      }
    }
    
    // Resolve conflicts for each node
    for node in nodes {
      let mut resolved_data = []
      
      for item in node.data {
        let latest_timestamp = key_timestamps[item.0]
        
        // Keep only the latest version of each key
        let mut is_latest = true
        
        for other_item in node.data {
          if other_item.0 == item.0 && other_item != item {
            // Simplified: just check if this is the one we want to keep
            is_latest = false
            break
          }
        }
        
        if is_latest {
          resolved_data = resolved_data.push(item)
        }
      }
      
      resolved_nodes = resolved_nodes.push({
        id: node.id,
        partition_id: node.partition_id,
        data: resolved_data,
        last_seen: node.last_seen
      })
    }
    
    resolved_nodes
  }
  
  // Create conflicting scenario
  let mut conflicting_nodes = []
  for i in 0..5 {
    let node = healed_nodes[i]
    
    if i < 3 {
      // First three nodes have one value
      let mut conflict_data = []
      
      for item in node.data {
        if item.0 == "conflict-key" {
          conflict_data = conflict_data.push(("conflict-key", "value-from-majority"))
        } else {
          conflict_data = conflict_data.push(item)
        }
      }
      
      if not(conflict_data.any(fn(item) { item.0 == "conflict-key" })) {
        conflict_data = conflict_data.push(("conflict-key", "value-from-majority"))
      }
      
      conflicting_nodes = conflicting_nodes.push({
        id: node.id,
        partition_id: "partition-0",
        data: conflict_data,
        last_seen: 1640995500
      })
    } else {
      // Last two nodes have different value
      let mut conflict_data = []
      
      for item in node.data {
        if item.0 == "conflict-key" {
          conflict_data = conflict_data.push(("conflict-key", "value-from-minority"))
        } else {
          conflict_data = conflict_data.push(item)
        }
      }
      
      if not(conflict_data.any(fn(item) { item.0 == "conflict-key" })) {
        conflict_data = conflict_data.push(("conflict-key", "value-from-minority"))
      }
      
      conflicting_nodes = conflicting_nodes.push({
        id: node.id,
        partition_id: "partition-1",
        data: conflict_data,
        last_seen: 1640995600  // Later timestamp
      })
    }
  }
  
  // Resolve conflicts
  let resolved_nodes = resolve_conflicts(conflicting_nodes)
  
  // Verify conflicts are resolved (all nodes should have the same value)
  let mut conflict_value = None
  
  for node in resolved_nodes {
    for item in node.data {
      if item.0 == "conflict-key" {
        match conflict_value {
          Some(value) => assert_eq(item.1, value),
          None => conflict_value = Some(item.1)
        }
        break
      }
    }
  }
  
  // Should have resolved to the value with later timestamp
  match conflict_value {
    Some(value) => assert_eq(value, "value-from-minority"),
    None => assert_true(false)
  }
  
  // Test partition tolerance metrics
  let calculate_partition_tolerance_metrics = fn(partitions: Array[Partition]) {
    let total_nodes = partitions.reduce(fn(acc, p) { acc + p.nodes.length() }, 0)
    let majority_partitions = partitions.filter(fn(p) { p.is_majority }).length()
    let minority_partitions = partitions.length() - majority_partitions
    
    let largest_partition_size = partitions.reduce(fn(acc, p) { 
      if p.nodes.length() > acc { p.nodes.length() } else { acc } 
    }, 0)
    
    let partition_ratio = largest_partition_size.to_float() / total_nodes.to_float()
    
    {
      total_nodes,
      partition_count: partitions.length(),
      majority_partitions,
      minority_partitions,
      largest_partition_size,
      partition_ratio
    }
  }
  
  let tolerance_metrics = calculate_partition_tolerance_metrics(partitions)
  
  assert_eq(tolerance_metrics.total_nodes, 5)
  assert_eq(tolerance_metrics.partition_count, 2)
  assert_eq(tolerance_metrics.majority_partitions, 1)
  assert_eq(tolerance_metrics.minority_partitions, 1)
  assert_eq(tolerance_metrics.largest_partition_size, 3)
  assert_eq(tolerance_metrics.partition_ratio, 3.0 / 5.0)
  
  // Test availability during partition
  let calculate_availability = fn(nodes: Array[PartitionedNode>, partition_id: String) {
    let partition_nodes = nodes.filter(fn(n) { n.partition_id == partition_id })
    let total_nodes = nodes.length()
    
    if total_nodes == 0 {
      0.0
    } else {
      partition_nodes.length().to_float() / total_nodes.to_float()
    }
  }
  
  let majority_availability = calculate_availability(partitioned_nodes_with_partitions, "partition-0")
  let minority_availability = calculate_availability(partitioned_nodes_with_partitions, "partition-1")
  
  assert_eq(majority_availability, 3.0 / 5.0)  // 3 out of 5 nodes
  assert_eq(minority_availability, 2.0 / 5.0)   // 2 out of 5 nodes
  
  // Majority partition should have higher availability
  assert_true(majority_availability > minority_availability)
}

// Test 10: Distributed System Failure Recovery
test "distributed system failure recovery" {
  // Failure and recovery structures
  type NodeState = {
    id: String,
    status: String,  // "active", "failed", "recovering"
    data: Array[(String, String)],
    last_heartbeat: Int,
    checkpoint: Option[Int>
  }
  
  type FailureEvent = {
    node_id: String,
    failure_type: String,
    timestamp: Int,
    detected_by: Array[String]
  }
  
  type RecoveryPlan = {
    failed_node: String,
    recovery_strategy: String,
    source_nodes: Array[String>,
    estimated_time: Int
  }
  
  // Create initial nodes
  let create_nodes = fn(count: Int) {
    let mut nodes = []
    
    for i in 0..count {
      nodes = nodes.push({
        id: "node-" + i.to_string(),
        status: "active",
        data: [],
        last_heartbeat: 1640995200,
        checkpoint: Some(1640995200)
      })
    }
    
    nodes
  }
  
  let nodes = create_nodes(5)
  assert_eq(nodes.length(), 5)
  
  // Simulate node failure
  let simulate_failure = fn(nodes: Array[NodeState>, node_id: String, failure_type: String, timestamp: Int) {
    let mut updated_nodes = []
    let mut detectors = []
    
    for node in nodes {
      if node.id == node_id {
        updated_nodes = updated_nodes.push({
          id: node.id,
          status: "failed",
          data: node.data,
          last_heartbeat: timestamp,
          checkpoint: node.checkpoint
        })
      } else {
        updated_nodes = updated_nodes.push({
          id: node.id,
          status: node.status,
          data: node.data,
          last_heartbeat: node.last_heartbeat,
          checkpoint: node.checkpoint
        })
        
        // Other nodes detect the failure
        detectors = detectors.push(node.id)
      }
    }
    
    (updated_nodes, {
      node_id,
      failure_type,
      timestamp,
      detected_by: detectors
    })
  }
  
  // Test node failure
  let (failed_nodes, failure_event) = simulate_failure(nodes, "node-2", "network_partition", 1640995300)
  
  assert_eq(failure_event.node_id, "node-2")
  assert_eq(failure_event.failure_type, "network_partition")
  assert_eq(failure_event.timestamp, 1640995300)
  assert_eq(failure_event.detected_by.length(), 4)  // All other nodes detect the failure
  
  // Verify node status updated
  for node in failed_nodes {
    if node.id == "node-2" {
      assert_eq(node.status, "failed")
      assert_eq(node.last_heartbeat, 1640995300)
    } else {
      assert_eq(node.status, "active")
    }
  }
  
  // Detect failures based on heartbeat
  let detect_failures = fn(nodes: Array[NodeState], current_time: Int, timeout: Int) {
    let mut detected_failures = []
    let mut updated_nodes = []
    
    for node in nodes {
      if node.status == "active" && current_time - node.last_heartbeat > timeout {
        // Node is considered failed
        let mut detectors = []
        
        for detector in nodes {
          if detector.id != node.id && detector.status == "active" {
            detectors = detectors.push(detector.id)
          }
        }
        
        detected_failures = detected_failures.push({
          node_id: node.id,
          failure_type: "heartbeat_timeout",
          timestamp: current_time,
          detected_by: detectors
        })
        
        updated_nodes = updated_nodes.push({
          id: node.id,
          status: "failed",
          data: node.data,
          last_heartbeat: node.last_heartbeat,
          checkpoint: node.checkpoint
        })
      } else {
        updated_nodes = updated_nodes.push(node)
      }
    }
    
    (updated_nodes, detected_failures)
  }
  
  // Test failure detection
  let mut heartbeat_nodes = failed_nodes
  
  // Update heartbeats for active nodes
  for i in 0..heartbeat_nodes.length() {
    if heartbeat_nodes[i].status == "active" {
      heartbeat_nodes[i] = { heartbeat_nodes[i] | last_heartbeat: 1640995400 }
    }
  }
  
  // Node-3 missed heartbeat
  heartbeat_nodes[3] = { heartbeat_nodes[3] | last_heartbeat: 1640995200 }
  
  let (detected_nodes, detected_failures) = detect_failures(heartbeat_nodes, 1640995500, 600)  // 10 minute timeout
  
  // Should detect node-3 as failed
  assert_true(detected_failures.length() >= 1)
  
  let node_3_failure = detected_failures.find(fn(f) { f.node_id == "node-3" })
  match node_3_failure {
    Some(failure) => {
      assert_eq(failure.failure_type, "heartbeat_timeout")
      assert_eq(failure.timestamp, 1640995500)
    }
    None => assert_true(false)
  }
  
  // Create recovery plan
  let create_recovery_plan = fn(failed_node: String, active_nodes: Array[NodeState]) {
    let source_nodes = active_nodes.filter(fn(n) { n.status == "active" }).map(fn(n) { n.id })
    
    {
      failed_node,
      recovery_strategy: "data_replication",
      source_nodes,
      estimated_time: 300  // 5 minutes
    }
  }
  
  // Test recovery plan creation
  let active_nodes = detected_nodes.filter(fn(n) { n.status == "active" })
  let recovery_plan = create_recovery_plan("node-2", active_nodes)
  
  assert_eq(recovery_plan.failed_node, "node-2")
  assert_eq(recovery_plan.recovery_strategy, "data_replication")
  assert_eq(recovery_plan.source_nodes.length(), 3)  // 3 active nodes
  assert_eq(recovery_plan.estimated_time, 300)
  
  // Execute recovery plan
  let execute_recovery = fn(nodes: Array[NodeState], plan: RecoveryPlan, current_time: Int) {
    let mut updated_nodes = []
    let mut recovered_data = []
    
    // Collect data from source nodes
    for node in nodes {
      if plan.source_nodes.contains(node.id) {
        for item in node.data {
          if not(recovered_data.contains(item)) {
            recovered_data = recovered_data.push(item)
          }
        }
      }
    }
    
    // Recover the failed node
    for node in nodes {
      if node.id == plan.failed_node {
        updated_nodes = updated_nodes.push({
          id: node.id,
          status: "recovering",
          data: recovered_data,
          last_heartbeat: current_time,
          checkpoint: Some(current_time)
        })
      } else {
        updated_nodes = updated_nodes.push(node)
      }
    }
    
    updated_nodes
  }
  
  // Add some data to active nodes
  let mut data_nodes = detected_nodes
  for i in 0..data_nodes.length() {
    if data_nodes[i].status == "active" {
      let node_data = ("key-" + i.to_string(), "value-" + i.to_string())
      data_nodes[i] = { data_nodes[i] | data: data_nodes[i].data.push(node_data) }
    }
  }
  
  // Execute recovery
  let recovering_nodes = execute_recovery(data_nodes, recovery_plan, 1640995600)
  
  // Verify node is recovering
  for node in recovering_nodes {
    if node.id == "node-2" {
      assert_eq(node.status, "recovering")
      assert_eq(node.last_heartbeat, 1640995600)
      assert_eq(node.checkpoint, Some(1640995600))
      
      // Verify data was recovered
      assert_true(node.data.length() > 0)
      
      // Should have data from all active nodes
      for i in 0..data_nodes.length() {
        if data_nodes[i].status == "active" {
          let expected_data = ("key-" + i.to_string(), "value-" + i.to_string())
          assert_true(node.data.contains(expected_data))
        }
      }
    }
  }
  
  // Complete recovery
  let complete_recovery = fn(nodes: Array[NodeState], node_id: String, current_time: Int) {
    let mut updated_nodes = []
    
    for node in nodes {
      if node.id == node_id {
        updated_nodes = updated_nodes.push({
          id: node.id,
          status: "active",
          data: node.data,
          last_heartbeat: current_time,
          checkpoint: Some(current_time)
        })
      } else {
        updated_nodes = updated_nodes.push(node)
      }
    }
    
    updated_nodes
  }
  
  // Complete recovery for node-2
  let recovered_nodes = complete_recovery(recovering_nodes, "node-2", 1640995900)
  
  // Verify node is active again
  for node in recovered_nodes {
    if node.id == "node-2" {
      assert_eq(node.status, "active")
      assert_eq(node.last_heartbeat, 1640995900)
      assert_eq(node.checkpoint, Some(1640995900))
    }
  }
  
  // Test checkpoint and rollback
  let create_checkpoint = fn(nodes: Array[NodeState], current_time: Int) {
    let mut updated_nodes = []
    
    for node in nodes {
      updated_nodes = updated_nodes.push({
        id: node.id,
        status: node.status,
        data: node.data,
        last_heartbeat: node.last_heartbeat,
        checkpoint: Some(current_time)
      })
    }
    
    updated_nodes
  }
  
  let rollback_to_checkpoint = fn(nodes: Array[NodeState], checkpoint_time: Int) {
    let mut updated_nodes = []
    
    for node in nodes {
      match node.checkpoint {
        Some(checkpoint) => {
          if checkpoint == checkpoint_time {
            // Rollback to checkpoint state
            updated_nodes = updated_nodes.push({
              id: node.id,
              status: "active",
              data: [],  // Reset to empty data for this test
              last_heartbeat: 1640995200,
              checkpoint: Some(checkpoint_time)
            })
          } else {
            updated_nodes = updated_nodes.push(node)
          }
        }
        None => updated_nodes = updated_nodes.push(node)
      }
    }
    
    updated_nodes
  }
  
  // Create checkpoint
  let checkpoint_nodes = create_checkpoint(recovered_nodes, 1640996000)
  
  // Verify checkpoint was created
  for node in checkpoint_nodes {
    assert_eq(node.checkpoint, Some(1640996000))
  }
  
  // Rollback to checkpoint
  let rolled_back_nodes = rollback_to_checkpoint(checkpoint_nodes, 1640996000)
  
  // Verify rollback
  for node in rolled_back_nodes {
    assert_eq(node.status, "active")
    assert_eq(node.data.length(), 0)  // Data was reset
    assert_eq(node.last_heartbeat, 1640995200)
    assert_eq(node.checkpoint, Some(1640996000))
  }
  
  // Test failure recovery metrics
  let calculate_recovery_metrics = fn(original_nodes: Array[NodeState>, recovered_nodes: Array[NodeState]) {
    let mut failed_count = 0
    let mut recovered_count = 0
    let mut data_loss = 0
    
    for i in 0..original_nodes.length() {
      let original = original_nodes[i]
      let recovered = recovered_nodes.find(fn(n) { n.id == original.id })
      
      match recovered {
        Some(node) => {
          if original.status == "failed" && node.status == "active" {
            recovered_count = recovered_count + 1
          }
          
          // Calculate data loss (simplified)
          if original.data.length() > node.data.length() {
            data_loss = data_loss + (original.data.length() - node.data.length())
          }
        }
        None => {}
      }
      
      if original.status == "failed" {
        failed_count = failed_count + 1
      }
    }
    
    {
      failed_count,
      recovered_count,
      recovery_rate: if failed_count > 0 { recovered_count.to_float() / failed_count.to_float() } else { 1.0 },
      data_loss
    }
  }
  
  // Simulate another failure and recovery for metrics
  let (failed_again_nodes, _) = simulate_failure(rolled_back_nodes, "node-1", "hardware_failure", 1640996100)
  let active_after_failure = failed_again_nodes.filter(fn(n) { n.status == "active" })
  let recovery_plan_again = create_recovery_plan("node-1", active_after_failure)
  let recovering_again_nodes = execute_recovery(failed_again_nodes, recovery_plan_again, 1640996200)
  let recovered_again_nodes = complete_recovery(recovering_again_nodes, "node-1", 1640996300)
  
  let recovery_metrics = calculate_recovery_metrics(failed_again_nodes, recovered_again_nodes)
  
  assert_eq(recovery_metrics.failed_count, 1)
  assert_eq(recovery_metrics.recovered_count, 1)
  assert_eq(recovery_metrics.recovery_rate, 1.0)
  
  // Test cascading failure detection
  let detect_cascading_failures = fn(nodes: Array[NodeState], time_window: Int, failure_threshold: Int) {
    let mut failure_counts = {}
    let mut cascading_failures = []
    
    // Count failures per time window
    for node in nodes {
      if node.status == "failed" {
        let time_bucket = (node.last_heartbeat / time_window) * time_window
        let count = match failure_counts[time_bucket] {
          Some(c) => c + 1,
          None => 1
        }
        
        failure_counts = failure_counts.insert(time_bucket, count)
      }
    }
    
    // Check for cascading failures
    for (time_bucket, count) in failure_counts {
      if count >= failure_threshold {
        cascading_failures = cascading_failups.push({
          time_window: time_bucket,
          failure_count: count,
          threshold_exceeded: true
        })
      }
    }
    
    cascading_failures
  }
  
  // Create multiple failures for testing
  let mut cascading_nodes = []
  for i in 0..5 {
    let node = recovered_again_nodes[i]
    
    if i < 3 {
      // First three nodes fail
      cascading_nodes = cascading_nodes.push({
        id: node.id,
        status: "failed",
        data: node.data,
        last_heartbeat: 1640996400 + i * 10,  // Close timestamps
        checkpoint: node.checkpoint
      })
    } else {
      cascading_nodes = cascading_nodes.push(node)
    }
  }
  
  let cascading_failures = detect_cascading_failures(cascading_nodes, 100, 2)  // 100s window, threshold of 2
  
  assert_true(cascading_failures.length() > 0)
  
  let cascade = cascading_failures[0]
  assert_true(cascade.failure_count >= 2)
  assert_true(cascade.threshold_exceeded)
}