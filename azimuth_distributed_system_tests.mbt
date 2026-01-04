// Azimuth Distributed System Test Suite
// 分布式系统测试套件 - 专注于分布式一致性、共识算法和分布式协调

// 测试1: 分布式一致性哈希
test "分布式一致性哈希测试" {
  // 创建一致性哈希环
  let hash_ring = ConsistentHashRing::new()
  ConsistentHashRing::set_virtual_nodes(hash_ring, 150) // 每个物理节点150个虚拟节点
  
  // 添加节点到哈希环
  let nodes = [
    "node-1:10.0.1.10:8080",
    "node-2:10.0.1.11:8080",
    "node-3:10.0.1.12:8080",
    "node-4:10.0.1.13:8080",
    "node-5:10.0.1.14:8080"
  ]
  
  for node in nodes {
    ConsistentHashRing::add_node(hash_ring, node)
  }
  
  // 验证节点添加
  assert_eq(ConsistentHashRing::get_node_count(hash_ring), 5)
  assert_eq(ConsistentHashRing::get_virtual_node_count(hash_ring), 5 * 150)
  
  // 测试键分布
  let test_keys = []
  for i in 1..=1000 {
    test_keys.push("trace-" + String::pad_left(i.to_string(), 6, "0"))
  }
  
  let key_distribution = {}
  for key in test_keys {
    let node = ConsistentHashRing::get_node(hash_ring, key)
    let count = key_distribution.get(node).or_else(0)
    key_distribution.set(node, count + 1)
  }
  
  // 验证键分布均匀性
  let expected_keys_per_node = test_keys.length() / nodes.length()
  let tolerance = expected_keys_per_node * 0.1 // 10%容差
  
  for node in nodes {
    let count = key_distribution.get(node)
    assert_true(count >= expected_keys_per_node - tolerance)
    assert_true(count <= expected_keys_per_node + tolerance)
  }
  
  // 测试节点添加时的数据迁移
  let new_node = "node-6:10.0.1.15:8080"
  let migration_keys = []
  
  // 记录添加新节点前的键分布
  let before_distribution = {}
  for key in test_keys.slice(0, 100) { // 只测试前100个键
    let node = ConsistentHashRing::get_node(hash_ring, key)
    before_distribution.set(key, node)
  }
  
  // 添加新节点
  ConsistentHashRing::add_node(hash_ring, new_node)
  assert_eq(ConsistentHashRing::get_node_count(hash_ring), 6)
  
  // 计算需要迁移的键
  let mut migrated_count = 0
  for key in test_keys.slice(0, 100) {
    let old_node = before_distribution.get(key)
    let new_node = ConsistentHashRing::get_node(hash_ring, key)
    
    if old_node != new_node {
      migrated_count = migrated_count + 1
      migration_keys.push((key, old_node, new_node))
    }
  }
  
  // 验证数据迁移最小化
  let expected_migration_rate = 1.0 / (nodes.length() + 1).to_float() // 约1/6的键需要迁移
  let actual_migration_rate = migrated_count.to_float() / 100.0
  
  assert_true(actual_migration_rate < expected_migration_rate + 0.05) // 允许5%误差
  
  // 测试节点移除时的数据重新分布
  let removed_node = "node-3:10.0.1.12:8080"
  let before_removal_distribution = {}
  
  for key in test_keys.slice(0, 100) {
    let node = ConsistentHashRing::get_node(hash_ring, key)
    before_removal_distribution.set(key, node)
  }
  
  // 移除节点
  ConsistentHashRing::remove_node(hash_ring, removed_node)
  assert_eq(ConsistentHashRing::get_node_count(hash_ring), 5)
  
  // 验证受影响的键被重新分配
  let mut redistributed_count = 0
  for key in test_keys.slice(0, 100) {
    let old_node = before_removal_distribution.get(key)
    let new_node = ConsistentHashRing::get_node(hash_ring, key)
    
    if old_node == removed_node {
      assert_true(new_node != removed_node) // 被移除的节点不应该再被分配
      redistributed_count = redistributed_count + 1
    } else {
      assert_eq(old_node, new_node) // 其他节点的键应该保持不变
    }
  }
  
  assert_true(redistributed_count > 0) // 应该有键被重新分配
  
  // 测试哈希环的连续性
  let ring_nodes = ConsistentHashRing::get_ring_nodes(hash_ring)
  assert_eq(ring_nodes.length(), ConsistentHashRing::get_virtual_node_count(hash_ring))
  
  // 验证环是有序的
  for i in 1..=ring_nodes.length() - 1 {
    assert_true(ring_nodes[i-1].hash <= ring_nodes[i].hash)
  }
  
  // 测试范围查询
  let test_key = "test-range-key"
  let target_node = ConsistentHashRing::get_node(hash_ring, test_key)
  let range_nodes = ConsistentHashRing::get_nodes_in_range(hash_ring, test_key, 2)
  
  assert_eq(range_nodes.length(), 2)
  assert_eq(range_nodes[0], target_node)
  assert_true(range_nodes[1] != target_node)
  
  // 测试复制因子
  let replication_factor = 3
  let replicated_nodes = ConsistentHashRing::get_replicated_nodes(hash_ring, test_key, replication_factor)
  
  assert_eq(replicated_nodes.length(), replication_factor)
  for i in 1..=replicated_nodes.length() - 1 {
    assert_true(replicated_nodes[i] != replicated_nodes[i-1]) // 节点应该不同
  }
}

// 测试2: 分布式锁服务
test "分布式锁服务测试" {
  // 创建分布式锁客户端
  let lock_client = DistributedLockClient::new()
  DistributedLockClient::set_connection_string(lock_client, "redis://localhost:6379")
  DistributedLockClient::set_default_timeout(lock_client, 30000) // 30秒默认超时
  DistributedLockClient::set_retry_interval(lock_client, 100) // 100ms重试间隔
  
  // 连接到锁服务
  let connect_result = DistributedLockClient::connect(lock_client)
  assert_true(connect_result.success)
  
  // 测试基本锁获取和释放
  let lock_key = "telemetry-processing-lock"
  let lock_value = "worker-" + Random::next_int(1000).to_string()
  
  let acquire_start = Clock::now_unix_nanos(Clock::system())
  let acquire_result = DistributedLockClient::acquire_lock(lock_client, lock_key, lock_value, 5000)
  let acquire_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(acquire_result.success)
  assert_eq(acquire_result.lock_key, lock_key)
  assert_eq(acquire_result.lock_value, lock_value)
  assert_true(acquire_result.ttl > 0)
  
  // 验证获取锁的时间
  let acquire_time = acquire_end - acquire_start
  assert_true(acquire_time < 1000000000) // 小于1秒
  
  // 测试锁重入
  let reentrant_result = DistributedLockClient::acquire_lock(lock_client, lock_key, lock_value, 5000)
  assert_true(reentrant_result.success) // 同一个客户端应该能重入
  
  // 测试锁竞争
  let competitor_lock_value = "competitor-" + Random::next_int(1000).to_string()
  let competitor_result = DistributedLockClient::try_acquire_lock(lock_client, lock_key, competitor_lock_value, 5000)
  
  assert_false(competitor_result.success) // 应该获取失败，因为锁已被占用
  
  // 测试锁释放
  let release_result = DistributedLockClient::release_lock(lock_client, lock_key, lock_value)
  assert_true(release_result.success)
  
  // 释放后，竞争者应该能获取锁
  let competitor_acquire_result = DistributedLockClient::acquire_lock(lock_client, lock_key, competitor_lock_value, 5000)
  assert_true(competitor_acquire_result.success)
  
  // 清理
  DistributedLockClient::release_lock(lock_client, lock_key, competitor_lock_value)
  
  // 测试锁超时
  let timeout_lock_key = "timeout-test-lock"
  let timeout_lock_value = "timeout-worker"
  
  let timeout_acquire_result = DistributedLockClient::acquire_lock(lock_client, timeout_lock_key, timeout_lock_value, 2000) // 2秒TTL
  assert_true(timeout_acquire_result.success)
  
  // 等待锁超时
  Thread::sleep(2500)
  
  // 超时后，其他客户端应该能获取锁
  let after_timeout_result = DistributedLockClient::try_acquire_lock(lock_client, timeout_lock_key, "new-worker", 5000)
  assert_true(after_timeout_result.success)
  
  // 清理
  DistributedLockClient::release_lock(lock_client, timeout_lock_key, "new-worker")
  
  // 测试锁续期
  let renewal_lock_key = "renewal-test-lock"
  let renewal_lock_value = "renewal-worker"
  
  let renewal_acquire_result = DistributedLockClient::acquire_lock(lock_client, renewal_lock_key, renewal_lock_value, 3000) // 3秒TTL
  assert_true(renewal_acquire_result.success)
  
  // 续期锁
  let renew_result = DistributedLockClient::renew_lock(lock_client, renewal_lock_key, renewal_lock_value, 5000) // 续期到5秒
  assert_true(renew_result.success)
  
  // 验证续期后的TTL
  assert_true(renew_result.new_ttl > 3000)
  
  // 清理
  DistributedLockClient::release_lock(lock_client, renewal_lock_key, renewal_lock_value)
  
  // 测试批量锁操作
  let batch_lock_keys = [
    "batch-lock-1",
    "batch-lock-2", 
    "batch-lock-3"
  ]
  
  let batch_lock_value = "batch-worker"
  let batch_acquire_result = DistributedLockClient::acquire_batch_locks(lock_client, batch_lock_keys, batch_lock_value, 5000)
  
  assert_true(batch_acquire_result.success)
  assert_eq(batch_acquire_result.acquired_locks.length(), batch_lock_keys.length())
  
  // 测试批量锁释放
  let batch_release_result = DistributedLockClient::release_batch_locks(lock_client, batch_lock_keys, batch_lock_value)
  assert_true(batch_release_result.success)
  
  // 测试锁状态查询
  let status_lock_key = "status-test-lock"
  let status_lock_value = "status-worker"
  
  DistributedLockClient::acquire_lock(lock_client, status_lock_key, status_lock_value, 10000)
  
  let lock_status = DistributedLockClient::get_lock_status(lock_client, status_lock_key)
  assert_true(lock_status.is_locked)
  assert_eq(lock_status.lock_value, status_lock_value)
  assert_true(lock_status.remaining_ttl > 0)
  
  // 清理
  DistributedLockClient::release_lock(lock_client, status_lock_key, status_lock_value)
  
  // 测试锁等待和阻塞
  let blocking_lock_key = "blocking-test-lock"
  
  // 第一个客户端获取锁
  let first_client = DistributedLockClient::new()
  DistributedLockClient::set_connection_string(first_client, "redis://localhost:6379")
  DistributedLockClient::connect(first_client)
  
  let first_acquire_result = DistributedLockClient::acquire_lock(first_client, blocking_lock_key, "client-1", 10000)
  assert_true(first_acquire_result.success)
  
  // 第二个客户端等待锁
  let second_client = DistributedLockClient::new()
  DistributedLockClient::set_connection_string(second_client, "redis://localhost:6379")
  DistributedLockClient::connect(second_client)
  
  let blocking_start = Clock::now_unix_nanos(Clock::system())
  
  // 启动后台任务获取锁
  let blocking_task = Task::spawn(fn() {
    DistributedLockClient::acquire_lock(second_client, blocking_lock_key, "client-2", 15000)
  })
  
  // 等待一段时间后释放锁
  Thread::sleep(2000)
  DistributedLockClient::release_lock(first_client, blocking_lock_key, "client-1")
  
  // 等待第二客户端获取锁
  let blocking_result = Task::await(blocking_task)
  let blocking_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(blocking_result.success)
  assert_eq(blocking_result.lock_value, "client-2")
  
  // 验证等待时间
  let blocking_time = blocking_end - blocking_start
  assert_true(blocking_time >= 2000000000) // 至少等待2秒
  
  // 清理
  DistributedLockClient::release_lock(second_client, blocking_lock_key, "client-2")
  
  // 关闭客户端连接
  DistributedLockClient::close(lock_client)
  DistributedLockClient::close(first_client)
  DistributedLockClient::close(second_client)
}

// 测试3: Raft共识算法
test "Raft共识算法测试" {
  // 创建Raft集群配置
  let cluster_config = RaftClusterConfig::new()
  RaftClusterConfig::set_cluster_id(cluster_config, "azimuth-telemetry-cluster")
  RaftClusterConfig::set_election_timeout(cluster_config, 5000) // 5秒选举超时
  RaftClusterConfig::set_heartbeat_interval(cluster_config, 1000) // 1秒心跳间隔
  RaftClusterConfig::set_log_compaction_threshold(cluster_config, 1000) // 1000条日志后压缩
  
  // 创建3个Raft节点
  let node_configs = [
    ("node-1", "127.0.0.1:9001", ["127.0.0.1:9002", "127.0.0.1:9003"]),
    ("node-2", "127.0.0.1:9002", ["127.0.0.1:9001", "127.0.0.1:9003"]),
    ("node-3", "127.0.0.1:9003", ["127.0.0.1:9001", "127.0.0.1:9002"])
  ]
  
  let raft_nodes = []
  for (node_id, address, peers) in node_configs {
    let node_config = RaftNodeConfig::new(node_id, address)
    RaftNodeConfig::set_peers(node_config, peers)
    RaftNodeConfig::set_data_dir(node_config, "/tmp/raft-" + node_id)
    
    let raft_node = RaftNode::new(node_config)
    let init_result = RaftNode::initialize(raft_node, cluster_config)
    assert_true(init_result.success)
    
    raft_nodes.push(raft_node)
  }
  
  // 启动所有节点
  for node in raft_nodes {
    let start_result = RaftNode::start(node)
    assert_true(start_result.success)
  }
  
  // 等待leader选举
  Thread::sleep(10000) // 等待10秒
  
  // 检查leader选举结果
  let mut leader_node = None
  let mut follower_nodes = []
  
  for node in raft_nodes {
    let state = RaftNode::get_state(node)
    if state.role == RaftRole::Leader {
      leader_node = Some(node)
    } else {
      follower_nodes.push(node)
    }
  }
  
  // 验证只有一个leader
  assert_true(leader_node.is_some())
  assert_eq(follower_nodes.length(), 2)
  
  let leader = leader_node.unwrap()
  
  // 测试leader提交日志
  let log_entry = RaftLogEntry::new()
  RaftLogEntry::set_command(log_entry, "SET telemetry.config.sampling_rate 0.1")
  RaftLogEntry::set_term(log_entry, RaftNode::get_current_term(leader))
  
  let propose_start = Clock::now_unix_nanos(Clock::system())
  let propose_result = RaftNode::propose(leader, log_entry)
  let propose_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(propose_result.success)
  assert_eq(propose_result.log_index, 1)
  assert_eq(propose_result.commit_index, 1)
  
  // 验证提交时间
  let propose_time = propose_end - propose_start
  assert_true(propose_time < 3000000000) // 小于3秒
  
  // 验证follower的日志同步
  Thread::sleep(2000) // 等待日志同步
  
  for follower in follower_nodes {
    let follower_state = RaftNode::get_state(follower)
    assert_eq(follower_state.commit_index, 1)
    
    let log_entry = RaftNode::get_log_entry(follower, 1)
    assert_true(log_entry.is_some())
    assert_eq(log_entry.unwrap().command, "SET telemetry.config.sampling_rate 0.1")
  }
  
  // 测试多个日志条目
  let commands = [
    "SET telemetry.config.export_interval 60",
    "SET telemetry.config.max_spans_per_batch 1000",
    "SET telemetry.config.compression true"
  ]
  
  for (i, command) in commands.enumerate() {
    let log_entry = RaftLogEntry::new()
    RaftLogEntry::set_command(log_entry, command)
    RaftLogEntry::set_term(log_entry, RaftNode::get_current_term(leader))
    
    let propose_result = RaftNode::propose(leader, log_entry)
    assert_true(propose_result.success)
    assert_eq(propose_result.log_index, i + 2) // 索引从2开始
  }
  
  // 等待日志同步
  Thread::sleep(3000)
  
  // 验证所有日志已提交
  let leader_state = RaftNode::get_state(leader)
  assert_eq(leader_state.commit_index, 4) // 1 + 3个新命令
  
  for follower in follower_nodes {
    let follower_state = RaftNode::get_state(follower)
    assert_eq(follower_state.commit_index, 4)
  }
  
  // 测试leader故障转移
  let leader_id = RaftNode::get_node_id(leader)
  
  // 停止leader
  let stop_result = RaftNode::stop(leader)
  assert_true(stop_result.success)
  
  // 等待新的leader选举
  Thread::sleep(10000) // 等待10秒
  
  // 检查新的leader
  let mut new_leader = None
  let mut new_followers = []
  
  for follower in follower_nodes {
    let state = RaftNode::get_state(follower)
    if state.role == RaftRole::Leader {
      new_leader = Some(follower)
    } else {
      new_followers.push(follower)
    }
  }
  
  // 验证新leader选举
  assert_true(new_leader.is_some())
  assert_eq(new_followers.length(), 1)
  
  let elected_leader = new_leader.unwrap()
  let elected_leader_id = RaftNode::get_node_id(elected_leader)
  assert_true(elected_leader_id != leader_id) // 新leader应该不同
  
  // 测试新leader提交日志
  let recovery_log_entry = RaftLogEntry::new()
  RaftLogEntry::set_command(recovery_log_entry, "SET telemetry.config.recovery_mode true")
  RaftLogEntry::set_term(recovery_log_entry, RaftNode::get_current_term(elected_leader))
  
  let recovery_result = RaftNode::propose(elected_leader, recovery_log_entry)
  assert_true(recovery_result.success)
  
  // 等待日志同步
  Thread::sleep(2000)
  
  // 验证日志已提交
  let new_leader_state = RaftNode::get_state(elected_leader)
  assert_eq(new_leader_state.commit_index, 5)
  
  // 测试日志快照
  let snapshot_result = RaftNode::create_snapshot(elected_leader)
  assert_true(snapshot_result.success)
  assert_true(snapshot_result.snapshot_file.length() > 0)
  
  // 测试从快照恢复
  for node in [elected_leader] + new_followers {
    let restore_result = RaftNode::restore_from_snapshot(node, snapshot_result.snapshot_file)
    assert_true(restore_result.success)
  }
  
  // 清理资源
  for node in raft_nodes {
    RaftNode::stop(node)
  }
}

// 测试4: 分布式配置管理
test "分布式配置管理测试" {
  // 创建分布式配置客户端
  let config_client = DistributedConfigClient::new()
  DistributedConfigClient::set_connection_string(config_client, "consul://localhost:8500")
  DistributedConfigClient::set_data_center(config_client, "dc1")
  DistributedConfigClient::set_token(config_client, "consul-token-123456")
  
  // 连接到配置中心
  let connect_result = DistributedConfigClient::connect(config_client)
  assert_true(connect_result.success)
  
  // 测试配置写入
  let config_key = "azimuth/telemetry/config"
  let config_value = {
    "sampling_rate": 0.1,
    "export_interval": 60,
    "max_spans_per_batch": 1000,
    "compression": {
      "enabled": true,
      "algorithm": "gzip",
      "level": 6
    },
    "services": {
      "collector": {
        "endpoint": "http://collector:4317",
        "timeout": 5000
      },
      "storage": {
        "type": "elasticsearch",
        "endpoint": "http://elasticsearch:9200"
      }
    }
  }
  
  let put_start = Clock::now_unix_nanos(Clock::system())
  let put_result = DistributedConfigClient::put(config_client, config_key, Json::serialize(config_value))
  let put_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(put_result.success)
  assert_true(put_result.modify_index > 0)
  
  // 验证写入时间
  let put_time = put_end - put_start
  assert_true(put_time < 2000000000) // 小于2秒
  
  // 测试配置读取
  let get_start = Clock::now_unix_nanos(Clock::system())
  let get_result = DistributedConfigClient::get(config_client, config_key)
  let get_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(get_result.success)
  assert_eq(get_result.value, Json::serialize(config_value))
  assert_true(get_result.modify_index == put_result.modify_index)
  
  // 验证读取时间
  let get_time = get_end - get_start
  assert_true(get_time < 1000000000) // 小于1秒
  
  // 测试配置监听
  let watch_result = DistributedConfigClient::watch(config_client, config_key)
  assert_true(watch_result.success)
  
  let watcher = watch_result.watcher
  
  // 修改配置
  let updated_config = config_value
  updated_config.set("sampling_rate", 0.2) // 修改采样率
  
  Thread::sleep(1000) // 等待1秒
  
  let update_result = DistributedConfigClient::put(config_client, config_key, Json::serialize(updated_config))
  assert_true(update_result.success)
  
  // 等待配置变更通知
  let watch_start = Clock::now_unix_nanos(Clock::system())
  let change_notification = DistributedConfigClient::wait_for_change(watcher, 5000) // 5秒超时
  let watch_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(change_notification.is_some())
  
  let notification = change_notification.unwrap()
  assert_eq(notification.key, config_key)
  assert_true(notification.old_value != notification.new_value)
  assert_true(notification.new_value.contains("0.2")) // 新值应该包含更新后的采样率
  
  // 验证通知时间
  let notification_time = watch_end - watch_start
  assert_true(notification_time < 5000000000) // 小于5秒
  
  // 测试配置前缀查询
  let prefix_key = "azimuth/telemetry/"
  let prefix_result = DistributedConfigClient::get_prefix(config_client, prefix_key)
  
  assert_true(prefix_result.success)
  assert_true(prefix_result.kv_pairs.length() >= 1)
  
  let found_config = prefix_result.kv_pairs.some(fn(pair) { pair.key == config_key })
  assert_true(found_config)
  
  // 测试配置删除
  let delete_result = DistributedConfigClient::delete(config_client, config_key)
  assert_true(delete_result.success)
  
  // 验证删除
  let verify_delete_result = DistributedConfigClient::get(config_client, config_key)
  assert_false(verify_delete_result.success)
  
  // 测试配置事务
  let tx = DistributedConfigClient::create_transaction(config_client)
  
  let tx_operations = [
    ("azimuth/telemetry/sampling_rate", "0.15"),
    ("azimuth/telemetry/export_interval", "30"),
    ("azimuth/telemetry/max_spans", "500")
  ]
  
  for (key, value) in tx_operations {
    DistributedConfigClient::transaction_put(tx, key, value)
  }
  
  let tx_result = DistributedConfigClient::commit_transaction(tx)
  assert_true(tx_result.success)
  
  // 验证事务结果
  for (key, value) in tx_operations {
    let verify_result = DistributedConfigClient::get(config_client, key)
    assert_true(verify_result.success)
    assert_eq(verify_result.value, value)
  }
  
  // 测试配置锁
  let lock_key = "azimuth/telemetry/.lock"
  let lock_value = "config-manager-" + Random::next_int(1000).to_string()
  
  let lock_result = DistributedConfigClient::acquire_lock(config_client, lock_key, lock_value, 10) // 10秒锁
  assert_true(lock_result.success)
  
  // 测试锁竞争
  let competitor_lock_value = "competitor-" + Random::next_int(1000).to_string()
  let competitor_result = DistributedConfigClient::try_lock(config_client, lock_key, competitor_lock_value)
  assert_false(competitor_result.success) // 应该获取失败
  
  // 释放锁
  let unlock_result = DistributedConfigClient::release_lock(config_client, lock_key, lock_value)
  assert_true(unlock_result.success)
  
  // 释放后应该能获取锁
  let after_release_result = DistributedConfigClient::try_lock(config_client, lock_key, competitor_lock_value)
  assert_true(after_release_result.success)
  
  // 清理
  DistributedConfigClient::release_lock(config_client, lock_key, competitor_lock_value)
  
  for (key, _) in tx_operations {
    DistributedConfigClient::delete(config_client, key)
  }
  
  // 关闭客户端
  DistributedConfigClient::close(config_client)
}

// 测试5: 分布式消息队列
test "分布式消息队列测试" {
  // 创建消息队列客户端
  let mq_client = MessageQueueClient::new()
  MessageQueueClient::set_connection_string(mq_client, "amqp://guest:guest@localhost:5672")
  MessageQueueClient::set_virtual_host(mq_client, "azimuth")
  MessageQueueClient::set_heartbeat_interval(mq_client, 60) // 60秒心跳
  
  // 连接到消息队列
  let connect_result = MessageQueueClient::connect(mq_client)
  assert_true(connect_result.success)
  
  // 声明交换机
  let exchange_name = "azimuth.telemetry.exchange"
  let exchange_config = ExchangeConfig::new(ExchangeType::Topic, true, false)
  
  let declare_exchange_result = MessageQueueClient::declare_exchange(mq_client, exchange_name, exchange_config)
  assert_true(declare_exchange_result.success)
  
  // 声明队列
  let queue_configs = [
    ("azimuth.telemetry.spans", QueueConfig::new(true, false, false)),
    ("azimuth.telemetry.metrics", QueueConfig::new(true, false, false)),
    ("azimuth.telemetry.logs", QueueConfig::new(true, false, false))
  ]
  
  let declared_queues = []
  for (queue_name, queue_config) in queue_configs {
    let declare_result = MessageQueueClient::declare_queue(mq_client, queue_name, queue_config)
    assert_true(declare_result.success)
    declared_queues.push(queue_name)
  }
  
  // 绑定队列到交换机
  let bindings = [
    ("azimuth.telemetry.spans", "span.*"),
    ("azimuth.telemetry.metrics", "metric.*"),
    ("azimuth.telemetry.logs", "log.*")
  ]
  
  for (queue_name, routing_key) in bindings {
    let bind_result = MessageQueueClient::bind_queue(mq_client, exchange_name, queue_name, routing_key)
    assert_true(bind_result.success)
  }
  
  // 测试消息发布
  let telemetry_messages = [
    ("span.created", {
      "trace_id": "trace-mq-001",
      "span_id": "span-mq-001",
      "operation_name": "http.request",
      "service_name": "azimuth-telemetry",
      "start_time": 1640995200000,
      "end_time": 1640995200500,
      "status": "ok"
    }),
    ("metric.recorded", {
      "metric_name": "cpu.usage",
      "metric_value": 75.5,
      "metric_unit": "percent",
      "timestamp": 1640995200000,
      "tags": {
        "host": "server-01",
        "service": "azimuth-telemetry"
      }
    }),
    ("log.generated", {
      "level": "info",
      "message": "Telemetry processing started",
      "timestamp": 1640995200000,
      "service": "azimuth-telemetry",
      "trace_id": "trace-mq-001"
    })
  ]
  
  let publish_start = Clock::now_unix_nanos(Clock::system())
  
  for (routing_key, message) in telemetry_messages {
    let message_config = MessageConfig::new(Json::serialize(message))
    message_config.set_content_type("application/json")
    message_config.set_delivery_mode(DeliveryMode::Persistent)
    message_config.set_priority(5)
    
    let publish_result = MessageQueueClient::publish(mq_client, exchange_name, routing_key, message_config)
    assert_true(publish_result.success)
  }
  
  let publish_end = Clock::now_unix_nanos(Clock::system())
  let publish_time = publish_end - publish_start
  
  // 验证发布时间
  assert_true(publish_time < 2000000000) // 小于2秒
  
  // 测试消息消费
  let consumer_configs = [
    ("span-consumer", "azimuth.telemetry.spans", ["span.*"]),
    ("metric-consumer", "azimuth.telemetry.metrics", ["metric.*"]),
    ("log-consumer", "azimuth.telemetry.logs", ["log.*"])
  ]
  
  let consumers = []
  let consumed_messages = []
  
  for (consumer_name, queue_name, routing_keys) in consumer_configs {
    let consumer_config = ConsumerConfig::new(consumer_name, queue_name)
    consumer_config.set_auto_ack(false)
    consumer_config.set_prefetch_count(10)
    
    let consumer = MessageQueueClient::create_consumer(mq_client, consumer_config)
    assert_true(consumer.is_some())
    
    consumers.push(consumer.unwrap())
    consumed_messages.push([])
  }
  
  // 消费消息
  let consume_start = Clock::now_unix_nanos(Clock::system())
  
  for i in 0..=consumers.length() - 1 {
    let consumer = consumers[i]
    
    // 等待消息
    let consume_result = MessageQueueClient::consume(consumer, 5000) // 5秒超时
    assert_true(consume_result.success)
    
    let message = consume_result.message
    consumed_messages[i] = consumed_messages[i].push(message)
    
    // 确认消息
    let ack_result = MessageQueueClient::ack(consumer, message.delivery_tag)
    assert_true(ack_result.success)
  }
  
  let consume_end = Clock::now_unix_nanos(Clock::system())
  let consume_time = consume_end - consume_start
  
  // 验证消费时间
  assert_true(consume_time < 10000000000) // 小于10秒
  
  // 验证消息内容
  assert_eq(consumed_messages[0].length(), 1) // span队列应该有1条消息
  assert_eq(consumed_messages[1].length(), 1) // metric队列应该有1条消息
  assert_eq(consumed_messages[2].length(), 1) // log队列应该有1条消息
  
  // 验证消息内容完整性
  let span_message = consumed_messages[0][0]
  assert_true(span_message.body.contains("trace-mq-001"))
  assert_eq(span_message.routing_key, "span.created")
  assert_eq(span_message.content_type, "application/json")
  
  // 测试批量消费
  for i in 1..=5 {
    let message = {
      "batch_id": i,
      "message": "batch test message " + i.to_string(),
      "timestamp": 1640995200000 + i * 1000
    }
    
    let message_config = MessageConfig::new(Json::serialize(message))
    let publish_result = MessageQueueClient::publish(mq_client, exchange_name, "log.batch", message_config)
    assert_true(publish_result.success)
  }
  
  let log_consumer = consumers[2] // log队列的消费者
  
  // 批量消费
  let batch_consume_result = MessageQueueClient::consume_batch(log_consumer, 5, 5000)
  assert_true(batch_consume_result.success)
  assert_eq(batch_consume_result.messages.length(), 5)
  
  // 批量确认
  let delivery_tags = batch_consume_result.messages.map(fn(msg) { msg.delivery_tag })
  let batch_ack_result = MessageQueueClient::ack_batch(log_consumer, delivery_tags)
  assert_true(batch_ack_result.success)
  
  // 测试消息重试
  let retry_message = {
    "retry_count": 0,
    "message": "this will fail and retry",
    "timestamp": 1640995200000
  }
  
  let retry_config = MessageConfig::new(Json::serialize(retry_message))
  retry_config.set_priority(1) // 低优先级
  
  let retry_publish_result = MessageQueueClient::publish(mq_client, exchange_name, "test.retry", retry_config)
  assert_true(retry_publish_result.success)
  
  let retry_consumer = MessageQueueClient::create_consumer(mq_client, ConsumerConfig::new("retry-consumer", "azimuth.telemetry.spans"))
  
  let retry_consume_result = MessageQueueClient::consume(retry_consumer.unwrap(), 5000)
  assert_true(retry_consume_result.success)
  
  let retry_message_obj = retry_consume_result.message
  let retry_data = Json::parse(retry_message_obj.body)
  
  // 模拟处理失败，不确认消息
  Thread::sleep(2000)
  
  // 消息应该重新入队
  let retry_consume_again_result = MessageQueueClient::consume(retry_consumer.unwrap(), 5000)
  assert_true(retry_consume_again_result.success)
  
  // 现在确认消息
  let retry_ack_result = MessageQueueClient::ack(retry_consumer.unwrap(), retry_consume_again_result.message.delivery_tag)
  assert_true(retry_ack_result.success)
  
  // 测试消息TTL
  let ttl_message = {
    "message": "this will expire",
    "timestamp": 1640995200000
  }
  
  let ttl_config = MessageConfig::new(Json::serialize(ttl_message))
  ttl_config.set_ttl(2000) // 2秒TTL
  
  let ttl_publish_result = MessageQueueClient::publish(mq_client, exchange_name, "test.ttl", ttl_config)
  assert_true(ttl_publish_result.success)
  
  // 等待消息过期
  Thread::sleep(3000)
  
  // 尝试消费过期的消息（应该失败）
  let ttl_consumer = MessageQueueClient::create_consumer(mq_client, ConsumerConfig::new("ttl-consumer", "azimuth.telemetry.spans"))
  
  let ttl_consume_result = MessageQueueClient::consume(ttl_consumer.unwrap(), 2000)
  assert_false(ttl_consume_result.success) // 应该超时，因为消息已过期
  
  // 清理资源
  for consumer in consumers {
    MessageQueueClient::close_consumer(consumer)
  }
  
  for queue_name in declared_queues {
    MessageQueueClient::delete_queue(mq_client, queue_name)
  }
  
  MessageQueueClient::delete_exchange(mq_client, exchange_name)
  MessageQueueClient::close(mq_client)
}

// 测试6: 分布式缓存
test "分布式缓存测试" {
  // 创建分布式缓存客户端
  let cache_client = DistributedCacheClient::new()
  DistributedCacheClient::set_connection_string(cache_client, "redis://localhost:6379")
  DistributedCacheClient::set_default_ttl(cache_client, 3600) // 1小时默认TTL
  DistributedCacheClient::set_key_prefix(cache_client, "azimuth:telemetry:")
  
  // 连接到缓存
  let connect_result = DistributedCacheClient::connect(cache_client)
  assert_true(connect_result.success)
  
  // 测试基本缓存操作
  let cache_key = "trace-data:trace-001"
  let cache_value = {
    "trace_id": "trace-001",
    "span_count": 10,
    "total_duration": 5000,
    "service_names": ["azimuth-telemetry", "user-service", "order-service"]
  }
  
  let set_start = Clock::now_unix_nanos(Clock::system())
  let set_result = DistributedCacheClient::set(cache_client, cache_key, Json::serialize(cache_value))
  let set_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(set_result.success)
  
  // 验证设置时间
  let set_time = set_end - set_start
  assert_true(set_time < 100000000) // 小于100ms
  
  // 测试缓存获取
  let get_start = Clock::now_unix_nanos(Clock::system())
  let get_result = DistributedCacheClient::get(cache_client, cache_key)
  let get_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(get_result.success)
  assert_eq(get_result.value, Json::serialize(cache_value))
  assert_true(get_result.ttl > 0)
  
  // 验证获取时间
  let get_time = get_end - get_start
  assert_true(get_time < 50000000) // 小于50ms
  
  // 测试缓存不存在
  let non_existent_key = "non-existent-key"
  let non_existent_result = DistributedCacheClient::get(cache_client, non_existent_key)
  assert_false(non_existent_result.success)
  
  // 测试缓存删除
  let delete_result = DistributedCacheClient::delete(cache_client, cache_key)
  assert_true(delete_result.success)
  
  // 验证删除
  let verify_delete_result = DistributedCacheClient::get(cache_client, cache_key)
  assert_false(verify_delete_result.success)
  
  // 测试缓存TTL
  let ttl_key = "ttl-test-key"
  let ttl_value = "this will expire"
  
  let ttl_set_result = DistributedCacheClient::set_with_ttl(cache_client, ttl_key, ttl_value, 2) // 2秒TTL
  assert_true(ttl_set_result.success)
  
  // 立即获取应该成功
  let immediate_get_result = DistributedCacheClient::get(cache_client, ttl_key)
  assert_true(immediate_get_result.success)
  assert_eq(immediate_get_result.value, ttl_value)
  
  // 等待过期
  Thread::sleep(2500)
  
  // 过期后获取应该失败
  let expired_get_result = DistributedCacheClient::get(cache_client, ttl_key)
  assert_false(expired_get_result.success)
  
  // 测试批量操作
  let batch_keys = []
  let batch_values = []
  
  for i in 1..=10 {
    let key = "batch-key-" + i.to_string()
    let value = "batch-value-" + i.to_string()
    
    batch_keys.push(key)
    batch_values.push(value)
  }
  
  // 批量设置
  let batch_set_start = Clock::now_unix_nanos(Clock::system())
  let batch_set_result = DistributedCacheClient::mset(cache_client, batch_keys, batch_values)
  let batch_set_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(batch_set_result.success)
  
  // 验证批量设置时间
  let batch_set_time = batch_set_end - batch_set_start
  assert_true(batch_set_time < 200000000) // 小于200ms
  
  // 批量获取
  let batch_get_start = Clock::now_unix_nanos(Clock::system())
  let batch_get_result = DistributedCacheClient::mget(cache_client, batch_keys)
  let batch_get_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(batch_get_result.success)
  assert_eq(batch_get_result.values.length(), batch_keys.length())
  
  // 验证批量获取结果
  for i in 0..=batch_keys.length() - 1 {
    assert_eq(batch_get_result.values[i], batch_values[i])
  }
  
  // 验证批量获取时间
  let batch_get_time = batch_get_end - batch_get_start
  assert_true(batch_get_time < 100000000) // 小于100ms
  
  // 测试原子操作
  let atomic_key = "atomic-counter"
  
  // 初始化计数器
  let init_result = DistributedCacheClient::set(cache_client, atomic_key, "0")
  assert_true(init_result.success)
  
  // 原子递增
  let increment_result = DistributedCacheClient::increment(cache_client, atomic_key, 1)
  assert_true(increment_result.success)
  assert_eq(increment_result.new_value, 1)
  
  // 原子递增指定值
  let increment_by_result = DistributedCacheClient::increment_by(cache_client, atomic_key, 5)
  assert_true(increment_by_result.success)
  assert_eq(increment_by_result.new_value, 6)
  
  // 原子递减
  let decrement_result = DistributedCacheClient::decrement(cache_client, atomic_key, 1)
  assert_true(decrement_result.success)
  assert_eq(decrement_result.new_value, 5)
  
  // 测试条件操作
  let conditional_key = "conditional-key"
  let initial_value = "initial"
  
  DistributedCacheClient::set(cache_client, conditional_key, initial_value)
  
  // 条件设置（仅当键不存在时）
  let set_if_not_exists_result = DistributedCacheClient::set_if_not_exists(cache_client, conditional_key, "new-value")
  assert_false(set_if_not_exists_result.success) // 键已存在，应该失败
  
  // 删除键后再试
  DistributedCacheClient::delete(cache_client, conditional_key)
  
  let set_if_not_exists_result2 = DistributedCacheClient::set_if_not_exists(cache_client, conditional_key, "new-value")
  assert_true(set_if_not_exists_result2.success) // 键不存在，应该成功
  
  // 验证值
  let verify_result = DistributedCacheClient::get(cache_client, conditional_key)
  assert_true(verify_result.success)
  assert_eq(verify_result.value, "new-value")
  
  // 测试缓存统计
  let cache_stats = DistributedCacheClient::get_stats(cache_client)
  assert_true(cache_stats.total_operations > 0)
  assert_true(cache_stats.hit_count > 0)
  assert_true(cache_stats.miss_count >= 0)
  assert_true(cache_stats.hit_rate >= 0.0)
  assert_true(cache_stats.hit_rate <= 1.0)
  
  // 测试缓存模式
  let cache_pattern_keys = []
  let cache_pattern_values = []
  
  for i in 1..=100 {
    let key = "pattern:service-" + ((i % 5) + 1).to_string() + ":metric-" + i.to_string()
    let value = {
      "service": "service-" + ((i % 5) + 1).to_string(),
      "metric": "metric-" + i.to_string(),
      "value": Random::next_int(1000),
      "timestamp": 1640995200000 + i * 1000
    }
    
    cache_pattern_keys.push(key)
    cache_pattern_values.push(Json::serialize(value))
  }
  
  // 批量设置模式数据
  DistributedCacheClient::mset(cache_client, cache_pattern_keys, cache_pattern_values)
  
  // 按模式获取
  let pattern = "pattern:service-1:*"
  let pattern_result = DistributedCacheClient::get_by_pattern(cache_client, pattern)
  
  assert_true(pattern_result.success)
  assert_true(pattern_result.kv_pairs.length() >= 15) // service-1应该有约20个键
  
  // 验证模式结果
  for pair in pattern_result.kv_pairs {
    assert_true(pair.key.starts_with("pattern:service-1:"))
    
    let value_data = Json::parse(pair.value)
    assert_eq(value_data.get("service").to_string(), "service-1")
  }
  
  // 测试缓存分片
  let shard_client = DistributedCacheClient::new()
  DistributedCacheClient::set_connection_string(shard_client, "redis://localhost:6379")
  DistributedCacheClient::enable_sharding(shard_client, true)
  DistributedCacheClient::set_shard_count(shard_client, 4)
  
  let shard_connect_result = DistributedCacheClient::connect(shard_client)
  assert_true(shard_connect_result.success)
  
  // 在分片缓存中设置数据
  let shard_keys = []
  for i in 1..=20 {
    let key = "shard-key-" + i.to_string()
    let value = "shard-value-" + i.to_string()
    
    shard_keys.push(key)
    DistributedCacheClient::set(shard_client, key, value)
  }
  
  // 验证分片数据
  for key in shard_keys {
    let get_result = DistributedCacheClient::get(shard_client, key)
    assert_true(get_result.success)
    assert_true(get_result.value.contains("shard-value-"))
  }
  
  // 测试缓存预热
  let warmup_data = [
    ("config:sampling_rate", "0.1"),
    ("config:export_interval", "60"),
    ("config:max_spans", "1000"),
    ("config:compression", "true")
  ]
  
  let warmup_start = Clock::now_unix_nanos(Clock::system())
  let warmup_result = DistributedCacheClient::warmup(cache_client, warmup_data)
  let warmup_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(warmup_result.success)
  assert_eq(warmup_result.loaded_keys, 4)
  
  // 验证预热时间
  let warmup_time = warmup_end - warmup_start
  assert_true(warmup_time < 100000000) // 小于100ms
  
  // 验证预热数据
  for (key, value) in warmup_data {
    let get_result = DistributedCacheClient::get(cache_client, key)
    assert_true(get_result.success)
    assert_eq(get_result.value, value)
  }
  
  // 清理测试数据
  let all_keys = batch_keys + cache_pattern_keys + shard_keys
  for key in all_keys {
    DistributedCacheClient::delete(cache_client, key)
    DistributedCacheClient::delete(shard_client, key)
  }
  
  for (key, _) in warmup_data {
    DistributedCacheClient::delete(cache_client, key)
  }
  
  // 关闭客户端
  DistributedCacheClient::close(cache_client)
  DistributedCacheClient::close(shard_client)
}