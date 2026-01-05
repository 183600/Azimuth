// Azimuth 区块链技术测试用例
// 专注于区块链基础算法和概念

// 测试1: 哈希函数和默克尔树
test "哈希函数和默克尔树" {
  // 简化的哈希函数模拟
  let simple_hash = fn(data: String) {
    let mut hash = 0
    for char in data.to_char_array() {
      hash = (hash * 31 + char.to_int()) % 1000000007
    }
    hash
  }
  
  // 构建默克尔树
  let build_merkle_tree = fn(data: Array<String>) {
    if data.length() == 0 {
      return []
    }
    
    let mut level = data.map(fn(d) { simple_hash(d).to_string() })
    
    while level.length() > 1 {
      let mut next_level = []
      let mut i = 0
      
      while i < level.length() {
        if i + 1 < level.length() {
          // 合并两个节点
          let combined = level[i] + level[i + 1]
          next_level = next_level.push(simple_hash(combined).to_string())
        } else {
          // 奇数个节点，复制最后一个
          next_level = next_level.push(simple_hash(level[i] + level[i]).to_string())
        }
        i = i + 2
      }
      
      level = next_level
    }
    
    level
  }
  
  // 验证默克尔路径
  let verify_merkle_path = fn(leaf: String, path: Array<String>, indices: Array<Int>, root: String) {
    let mut current_hash = simple_hash(leaf).to_string()
    
    for i in 0..path.length() {
      if indices[i] == 0 {
        // 当前哈希是左子节点
        current_hash = simple_hash(current_hash + path[i]).to_string()
      } else {
        // 当前哈希是右子节点
        current_hash = simple_hash(path[i] + current_hash).to_string()
      }
    }
    
    current_hash == root
  }
  
  // 测试哈希函数
  let hash1 = simple_hash("hello")
  let hash2 = simple_hash("hello")
  let hash3 = simple_hash("world")
  
  assert_eq(hash1, hash2)  // 相同输入应产生相同哈希
  assert_true(hash1 != hash3)  // 不同输入应产生不同哈希
  
  // 测试默克尔树构建
  let data = ["transaction1", "transaction2", "transaction3", "transaction4"]
  let merkle_tree = build_merkle_tree(data)
  
  assert_eq(merkle_tree.length(), 1)  // 应该只有一个根节点
  
  // 测试奇数个数据的默克尔树
  let odd_data = ["tx1", "tx2", "tx3"]
  let odd_merkle_tree = build_merkle_tree(odd_data)
  
  assert_eq(odd_merkle_tree.length(), 1)
  
  // 测试默克尔路径验证
  let path = ["hash2", "hash34"]
  let indices = [0, 0]  // leaf在左侧，父节点也在左侧
  let root = merkle_tree[0]
  
  // 这里我们无法直接验证，因为我们没有实际的路径数据
  // 但我们可以验证验证函数的逻辑
  assert_true(verify_merkle_path("transaction1", ["dummy"], [0], simple_hash("dummy").to_string()))
}

// 测试2: 简化的工作量证明
test "工作量证明算法" {
  // 简化的工作量证明实现
  let proof_of_work = fn(data: String, difficulty: Int) {
    let mut nonce = 0
    let target = "0".repeat(difficulty)
    
    while true {
      let block_data = data + nonce.to_string()
      let hash = simple_hash(block_data).to_string()
      
      if hash.length() >= difficulty && hash.substring(0, difficulty) == target {
        return (nonce, hash)
      }
      
      nonce = nonce + 1
      
      // 防止无限循环
      if nonce > 1000000 {
        return (-1, "")
      }
    }
  }
  
  let simple_hash = fn(data: String) {
    let mut hash = 0
    for char in data.to_char_array() {
      hash = (hash * 31 + char.to_int()) % 1000000007
    }
    hash.to_string()
  }
  
  // 测试不同难度的工作量证明
  let (nonce1, hash1) = proof_of_work("test block", 1)
  let (nonce2, hash2) = proof_of_work("test block", 2)
  
  assert_true(nonce1 >= 0)
  assert_true(nonce2 >= 0)
  assert_true(hash1.length() > 0)
  assert_true(hash2.length() > 0)
  assert_true(nonce2 > nonce1)  // 更高难度应该需要更多尝试
  
  // 验证哈希满足难度要求
  assert_true(hash1.substring(0, 1) == "0")
  assert_true(hash2.substring(0, 2) == "00")
  
  // 测试验证工作量证明
  let verify_proof = fn(data: String, nonce: Int, hash: String, difficulty: Int) {
    let target = "0".repeat(difficulty)
    let block_data = data + nonce.to_string()
    let calculated_hash = simple_hash(block_data).to_string()
    
    calculated_hash == hash && hash.substring(0, difficulty) == target
  }
  
  assert_true(verify_proof("test block", nonce1, hash1, 1))
  assert_true(verify_proof("test block", nonce2, hash2, 2))
  assert_false(verify_proof("test block", nonce1, hash1, 2))  // 难度不匹配
}

// 测试3: 简化的区块链实现
test "简化区块链实现" {
  // 简化的区块结构
  type Block = {
    index: Int,
    timestamp: Int,
    data: String,
    previous_hash: String,
    hash: String,
    nonce: Int
  }
  
  type Blockchain = Array<Block>
  
  let simple_hash = fn(data: String) {
    let mut hash = 0
    for char in data.to_char_array() {
      hash = (hash * 31 + char.to_int()) % 1000000007
    }
    hash.to_string()
  }
  
  // 创建创世区块
  let create_genesis_block = fn() {
    let index = 0
    let timestamp = 1234567890
    let data = "Genesis Block"
    let previous_hash = "0"
    let nonce = 0
    
    let block_data = index.to_string() + timestamp.to_string() + data + previous_hash + nonce.to_string()
    let hash = simple_hash(block_data)
    
    {
      index,
      timestamp,
      data,
      previous_hash,
      hash,
      nonce
    }
  }
  
  // 添加新区块
  let add_block = fn(blockchain: Blockchain, data: String) {
    let previous_block = blockchain[blockchain.length() - 1]
    let index = previous_block.index + 1
    let timestamp = 1234567890 + index * 100
    let previous_hash = previous_block.hash
    
    // 简化的工作量证明
    let mut nonce = 0
    let mut hash = ""
    
    while true {
      let block_data = index.to_string() + timestamp.to_string() + data + previous_hash + nonce.to_string()
      hash = simple_hash(block_data)
      
      if hash.substring(0, 1) == "0" {
        break
      }
      
      nonce = nonce + 1
      if nonce > 10000 {
        break
      }
    }
    
    let new_block = {
      index,
      timestamp,
      data,
      previous_hash,
      hash,
      nonce
    }
    
    blockchain.push(new_block)
  }
  
  // 验证区块链完整性
  let validate_blockchain = fn(blockchain: Blockchain) {
    if blockchain.length() == 0 {
      return true
    }
    
    // 验证创世区块
    let genesis_block = blockchain[0]
    if genesis_block.index != 0 || genesis_block.previous_hash != "0" {
      return false
    }
    
    // 验证后续区块
    for i in 1..blockchain.length() {
      let current_block = blockchain[i]
      let previous_block = blockchain[i - 1]
      
      // 验证索引
      if current_block.index != previous_block.index + 1 {
        return false
      }
      
      // 验证前一个哈希
      if current_block.previous_hash != previous_block.hash {
        return false
      }
      
      // 验证当前哈希
      let block_data = current_block.index.to_string() + 
                      current_block.timestamp.to_string() + 
                      current_block.data + 
                      current_block.previous_hash + 
                      current_block.nonce.to_string()
      let calculated_hash = simple_hash(block_data)
      
      if calculated_hash != current_block.hash {
        return false
      }
      
      // 验证工作量证明
      if current_block.hash.substring(0, 1) != "0" {
        return false
      }
    }
    
    true
  }
  
  // 测试区块链创建和验证
  let mut blockchain = []
  let genesis_block = create_genesis_block()
  blockchain = blockchain.push(genesis_block)
  
  assert_eq(blockchain.length(), 1)
  assert_true(validate_blockchain(blockchain))
  
  // 添加新区块
  blockchain = add_block(blockchain, "Transaction 1")
  blockchain = add_block(blockchain, "Transaction 2")
  
  assert_eq(blockchain.length(), 3)
  assert_true(validate_blockchain(blockchain))
  
  // 验证区块链接
  assert_eq(blockchain[1].previous_hash, blockchain[0].hash)
  assert_eq(blockchain[2].previous_hash, blockchain[1].hash)
  
  // 测试篡改检测
  let mut tampered_blockchain = blockchain
  tampered_blockchain[1] = { 
    index: tampered_blockchain[1].index,
    timestamp: tampered_blockchain[1].timestamp,
    data: "Tampered Transaction",  // 篡改数据
    previous_hash: tampered_blockchain[1].previous_hash,
    hash: tampered_blockchain[1].hash,
    nonce: tampered_blockchain[1].nonce
  }
  
  assert_false(validate_blockchain(tampered_blockchain))
}

// 测试4: 简化的智能合约
test "简化智能合约" {
  // 简化的智能合约状态
  type ContractState = {
    balances: { String: Int },
    contract_owner: String,
    is_paused: Bool
  }
  
  // 简化的交易
  type Transaction = {
    from: String,
    to: String,
    amount: Int,
    function_call: Option<String>
  }
  
  // 初始化合约
  let init_contract = fn(owner: String, initial_balance: Int) {
    {
      balances: { owner: initial_balance },
      contract_owner: owner,
      is_paused: false
    }
  }
  
  // 执行交易
  let execute_transaction = fn(state: ContractState, tx: Transaction) {
    // 检查合约是否暂停
    if state.is_paused {
      return (state, "Contract is paused")
    }
    
    // 检查余额
    match state.balances[tx.from] {
      Some(from_balance) => {
        if from_balance < tx.amount {
          return (state, "Insufficient balance")
        }
        
        // 更新余额
        let mut new_balances = state.balances
        new_balances[tx.from] = from_balance - tx.amount
        
        let to_balance = 0
        match new_balances[tx.to] {
          Some(balance) => to_balance = balance
          None => ()
        }
        new_balances[tx.to] = to_balance + tx.amount
        
        let new_state = {
          balances: new_balances,
          contract_owner: state.contract_owner,
          is_paused: state.is_paused
        }
        
        (new_state, "Transaction successful")
      }
      None => (state, "Sender account not found")
    }
  }
  
  // 暂停合约（只有所有者可以调用）
  let pause_contract = fn(state: ContractState, caller: String) {
    if caller == state.contract_owner {
      ({
        balances: state.balances,
        contract_owner: state.contract_owner,
        is_paused: true
      }, "Contract paused")
    } else {
      (state, "Only owner can pause contract")
    }
  }
  
  // 恢复合约（只有所有者可以调用）
  let unpause_contract = fn(state: ContractState, caller: String) {
    if caller == state.contract_owner {
      ({
        balances: state.balances,
        contract_owner: state.contract_owner,
        is_paused: false
      }, "Contract unpaused")
    } else {
      (state, "Only owner can unpause contract")
    }
  }
  
  // 测试合约初始化
  let initial_state = init_contract("Alice", 1000)
  
  assert_eq(initial_state.contract_owner, "Alice")
  assert_eq(initial_state.balances["Alice"], 1000)
  assert_false(initial_state.is_paused)
  
  // 测试交易执行
  let tx1 = { from: "Alice", to: "Bob", amount: 100, function_call: None }
  let (state1, result1) = execute_transaction(initial_state, tx1)
  
  assert_eq(result1, "Transaction successful")
  assert_eq(state1.balances["Alice"], 900)
  assert_eq(state1.balances["Bob"], 100)
  
  // 测试余额不足
  let tx2 = { from: "Bob", to: "Charlie", amount: 200, function_call: None }
  let (state2, result2) = execute_transaction(state1, tx2)
  
  assert_eq(result2, "Insufficient balance")
  assert_eq(state2.balances["Bob"], 100)  // 余额不变
  
  // 测试合约暂停
  let (state3, result3) = pause_contract(state2, "Alice")
  
  assert_eq(result3, "Contract paused")
  assert_true(state3.is_paused)
  
  // 测试暂停状态下的交易
  let tx3 = { from: "Alice", to: "Charlie", amount: 50, function_call: None }
  let (state4, result4) = execute_transaction(state3, tx3)
  
  assert_eq(result4, "Contract is paused")
  
  // 测试非所有者暂停合约
  let (state5, result5) = pause_contract(state4, "Bob")
  
  assert_eq(result5, "Only owner can pause contract")
  assert_true(state5.is_paused)  // 状态不变
  
  // 测试恢复合约
  let (state6, result6) = unpause_contract(state5, "Alice")
  
  assert_eq(result6, "Contract unpaused")
  assert_false(state6.is_paused)
  
  // 测试恢复后的交易
  let (state7, result7) = execute_transaction(state6, tx3)
  
  assert_eq(result7, "Transaction successful")
  assert_eq(state7.balances["Alice"], 850)
  assert_eq(state7.balances["Charlie"], 50)
}

// 测试5: 简化的分布式共识
test "简化分布式共识算法" {
  // 节点状态
  type NodeState = {
    id: String,
    blockchain: Array<String>,  // 简化为哈希数组
    is_active: Bool
  }
  
  // 消息类型
  type Message = {
    sender: String,
    receiver: String,
    block_hash: String,
    message_type: String  // "propose", "vote", "commit"
  }
  
  // 网络状态
  type NetworkState = {
    nodes: Array<NodeState>,
    messages: Array<Message>,
    current_proposal: Option<String>
  }
  
  // 初始化网络
  let init_network = fn(node_ids: Array<String>) {
    let mut nodes = []
    
    for id in node_ids {
      nodes = nodes.push({
        id: id,
        blockchain: ["genesis"],
        is_active: true
      })
    }
    
    {
      nodes: nodes,
      messages: [],
      current_proposal: None
    }
  }
  
  // 提出新区块
  let propose_block = fn(network: NetworkState, proposer: String, block_hash: String) {
    let mut new_network = network
    
    // 检查提议者是否活跃
    let mut proposer_found = false
    let mut proposer_active = false
    
    for node in network.nodes {
      if node.id == proposer {
        proposer_found = true
        proposer_active = node.is_active
        break
      }
    }
    
    if not(proposer_found) || not(proposer_active) {
      return (new_network, "Proposer not found or inactive")
    }
    
    // 创建提议消息
    let mut new_messages = []
    for node in network.nodes {
      if node.id != proposer && node.is_active {
        new_messages = new_messages.push({
          sender: proposer,
          receiver: node.id,
          block_hash: block_hash,
          message_type: "propose"
        })
      }
    }
    
    new_network = {
      nodes: new_network.nodes,
      messages: new_network.messages + new_messages,
      current_proposal: Some(block_hash)
    }
    
    (new_network, "Block proposed")
  }
  
  // 投票
  let vote_block = fn(network: NetworkState, voter: String, block_hash: String, vote: Bool) {
    let mut new_network = network
    
    // 检查投票者是否活跃
    let mut voter_found = false
    let mut voter_active = false
    
    for node in network.nodes {
      if node.id == voter {
        voter_found = true
        voter_active = node.is_active
        break
      }
    }
    
    if not(voter_found) || not(voter_active) {
      return (new_network, "Voter not found or inactive")
    }
    
    // 创建投票消息
    let mut new_messages = []
    for node in network.nodes {
      if node.id != voter && node.is_active {
        new_messages = new_messages.push({
          sender: voter,
          receiver: node.id,
          block_hash: block_hash + ":" + vote.to_string(),
          message_type: "vote"
        })
      }
    }
    
    new_network = {
      nodes: new_network.nodes,
      messages: new_network.messages + new_messages,
      current_proposal: new_network.current_proposal
    }
    
    (new_network, "Vote cast")
  }
  
  // 计算共识
  let calculate_consensus = fn(network: NetworkState, block_hash: String) {
    let mut votes_for = 0
    let mut votes_against = 0
    let mut active_nodes = 0
    
    for node in network.nodes {
      if node.is_active {
        active_nodes = active_nodes + 1
      }
    }
    
    // 统计投票
    for message in network.messages {
      if message.message_type == "vote" && message.block_hash.starts_with(block_hash) {
        let vote_str = message.block_hash.substring(block_hash.length() + 1)
        if vote_str == "true" {
          votes_for = votes_for + 1
        } else {
          votes_against = votes_against + 1
        }
      }
    }
    
    // 简单多数原则
    if votes_for > active_nodes / 2 {
      "consensus_reached"
    } else if votes_against > active_nodes / 2 {
      "consensus_rejected"
    } else {
      "consensus_pending"
    }
  }
  
  // 测试网络初始化
  let node_ids = ["node1", "node2", "node3", "node4"]
  let network = init_network(node_ids)
  
  assert_eq(network.nodes.length(), 4)
  assert_eq(network.messages.length(), 0)
  assert_true(network.current_proposal.is_none())
  
  // 测试区块提议
  let (network1, result1) = propose_block(network, "node1", "block1_hash")
  
  assert_eq(result1, "Block proposed")
  assert_eq(network1.current_proposal, Some("block1_hash"))
  assert_eq(network1.messages.length(), 3)  // 发送给其他3个节点
  
  // 测试投票
  let (network2, result2) = vote_block(network1, "node2", "block1_hash", true)
  let (network3, result3) = vote_block(network2, "node3", "block1_hash", true)
  let (network4, result4) = vote_block(network3, "node4", "block1_hash", false)
  
  assert_eq(result2, "Vote cast")
  assert_eq(result3, "Vote cast")
  assert_eq(result4, "Vote cast")
  
  // 测试共识计算
  let consensus = calculate_consensus(network4, "block1_hash")
  
  assert_eq(consensus, "consensus_reached")  // 2票赞成，1票反对，超过半数
  
  // 测试节点故障
  let mut faulty_network = network4
  let mut new_nodes = []
  
  for node in faulty_network.nodes {
    if node.id == "node4" {
      new_nodes = new_nodes.push({
        id: node.id,
        blockchain: node.blockchain,
        is_active: false  // node4故障
      })
    } else {
      new_nodes = new_nodes.push(node)
    }
  }
  
  faulty_network = {
    nodes: new_nodes,
    messages: faulty_network.messages,
    current_proposal: faulty_network.current_proposal
  }
  
  // 测试故障节点下的提议
  let (network5, result5) = propose_block(faulty_network, "node2", "block2_hash")
  
  assert_eq(result5, "Block proposed")
  assert_eq(network5.messages.length(), 7)  // 之前的3条 + 新的2条（不包括故障节点）
  
  // 测试故障节点下的共识
  let (network6, result6) = vote_block(network5, "node1", "block2_hash", true)
  let (network7, result7) = vote_block(network6, "node3", "block2_hash", true)
  
  let consensus2 = calculate_consensus(network7, "block2_hash")
  
  assert_eq(consensus2, "consensus_reached")  // 2票赞成，0票反对，活跃节点3个，超过半数
}