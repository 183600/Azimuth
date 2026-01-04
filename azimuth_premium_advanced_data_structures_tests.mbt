// Azimuth Premium Advanced Data Structures and Algorithms Tests
// 高级数据结构和算法测试用例

// Test 1: 高效时间序列数据结构测试
test "efficient time series data structure operations" {
  // 创建时间序列数据结构
  let time_series = TimeSeries::new(100) // 最大容量100
  
  // 添加时间序列数据点
  TimeSeries::add_point(time_series, 1000L, 10.5)
  TimeSeries::add_point(time_series, 2000L, 15.3)
  TimeSeries::add_point(time_series, 3000L, 12.7)
  TimeSeries::add_point(time_series, 4000L, 18.2)
  TimeSeries::add_point(time_series, 5000L, 14.9)
  
  // 测试数据点数量
  assert_eq(TimeSeries::size(time_series), 5)
  
  // 测试按时间范围查询
  let range_data = TimeSeries::get_range(time_series, 2000L, 4000L)
  assert_eq(range_data.length(), 3)
  assert_eq(range_data[0].timestamp, 2000L)
  assert_eq(range_data[0].value, 15.3)
  assert_eq(range_data[2].timestamp, 4000L)
  assert_eq(range_data[2].value, 18.2)
  
  // 测试聚合操作
  let avg_value = TimeSeries::aggregate(time_series, 1000L, 5000L, Average)
  match avg_value {
    Some(value) => assert_eq(value, 14.32) // (10.5 + 15.3 + 12.7 + 18.2 + 14.9) / 5
    None => assert_true(false)
  }
  
  let max_value = TimeSeries::aggregate(time_series, 1000L, 5000L, Maximum)
  match max_value {
    Some(value) => assert_eq(value, 18.2)
    None => assert_true(false)
  }
  
  let min_value = TimeSeries::aggregate(time_series, 1000L, 5000L, Minimum)
  match min_value {
    Some(value) => assert_eq(value, 10.5)
    None => assert_true(false)
  }
  
  // 测试数据点淘汰（容量限制）
  for i in 1..=100 {
    TimeSeries::add_point(time_series, (5000L + i * 1000L), (10.0 + i as Float))
  }
  
  // 应该只保留最新的100个数据点
  assert_eq(TimeSeries::size(time_series), 100)
  assert_eq(TimeSeries::get_earliest_timestamp(time_series), 6000L)
}

// Test 2: 高效属性索引结构测试
test "efficient attribute index structure operations" {
  // 创建属性索引
  let attr_index = AttributeIndex::new()
  
  // 添加带属性的项
  let item1_id = "item1"
  let item1_attrs = [
    ("category", StringValue("sensor")),
    ("type", StringValue("temperature")),
    ("location", StringValue("room1")),
    ("status", StringValue("active"))
  ]
  AttributeIndex::add_item(attr_index, item1_id, item1_attrs)
  
  let item2_id = "item2"
  let item2_attrs = [
    ("category", StringValue("sensor")),
    ("type", StringValue("humidity")),
    ("location", StringValue("room1")),
    ("status", StringValue("active"))
  ]
  AttributeIndex::add_item(attr_index, item2_id, item2_attrs)
  
  let item3_id = "item3"
  let item3_attrs = [
    ("category", StringValue("actuator")),
    ("type", StringValue("valve")),
    ("location", StringValue("room2")),
    ("status", StringValue("inactive"))
  ]
  AttributeIndex::add_item(attr_index, item3_id, item3_attrs)
  
  // 测试单属性查询
  let sensor_items = AttributeIndex::query_by_attribute(attr_index, "category", StringValue("sensor"))
  assert_eq(sensor_items.length(), 2)
  assert_true(sensor_items.contains(item1_id))
  assert_true(sensor_items.contains(item2_id))
  
  // 测试复合属性查询
  let room1_sensors = AttributeIndex::query_by_multiple_attributes(
    attr_index,
    [("location", StringValue("room1")), ("category", StringValue("sensor"))]
  )
  assert_eq(room1_sensors.length(), 2)
  
  let active_sensors = AttributeIndex::query_by_multiple_attributes(
    attr_index,
    [("status", StringValue("active")), ("category", StringValue("sensor"))]
  )
  assert_eq(active_sensors.length(), 2)
  
  // 测试不存在的查询
  let non_existent = AttributeIndex::query_by_attribute(attr_index, "category", StringValue("nonexistent"))
  assert_eq(non_existent.length(), 0)
  
  // 测试属性更新
  AttributeIndex::update_attribute(attr_index, item1_id, "status", StringValue("maintenance"))
  let active_items = AttributeIndex::query_by_attribute(attr_index, "status", StringValue("active"))
  assert_eq(active_items.length(), 1) // 只有item2还是active
  assert_true(active_items.contains(item2_id))
  
  // 测试项删除
  AttributeIndex::remove_item(attr_index, item2_id)
  let remaining_sensors = AttributeIndex::query_by_attribute(attr_index, "category", StringValue("sensor"))
  assert_eq(remaining_sensors.length(), 1)
  assert_true(remaining_sensors.contains(item1_id))
}

// Test 3: 高效优先级队列操作测试
test "efficient priority queue operations" {
  // 创建优先级队列（最小堆）
  let priority_queue = PriorityQueue::new(10)
  
  // 添加元素（优先级，值）
  PriorityQueue::push(priority_queue, (5, "task5"))
  PriorityQueue::push(priority_queue, (2, "task2"))
  PriorityQueue::push(priority_queue, (8, "task8"))
  PriorityQueue::push(priority_queue, (1, "task1"))
  PriorityQueue::push(priority_queue, (7, "task7"))
  PriorityQueue::push(priority_queue, (3, "task3"))
  
  // 测试队列大小
  assert_eq(PriorityQueue::size(priority_queue), 6)
  
  // 测试按优先级出队（最小优先级先出）
  let (priority1, value1) = PriorityQueue::pop(priority_queue)
  assert_eq(priority1, 1)
  assert_eq(value1, "task1")
  
  let (priority2, value2) = PriorityQueue::pop(priority_queue)
  assert_eq(priority2, 2)
  assert_eq(value2, "task2")
  
  let (priority3, value3) = PriorityQueue::pop(priority_queue)
  assert_eq(priority3, 3)
  assert_eq(value3, "task3")
  
  // 测试查看顶部元素但不移除
  let (top_priority, top_value) = PriorityQueue::peek(priority_queue)
  assert_eq(top_priority, 5)
  assert_eq(top_value, "task5")
  
  // 队列大小应该不变
  assert_eq(PriorityQueue::size(priority_queue), 3)
  
  // 测试更新元素优先级
  PriorityQueue::update_priority(priority_queue, "task8", 0)
  let (new_top_priority, new_top_value) = PriorityQueue::pop(priority_queue)
  assert_eq(new_top_priority, 0)
  assert_eq(new_top_value, "task8")
  
  // 测试清空队列
  PriorityQueue::clear(priority_queue)
  assert_eq(PriorityQueue::size(priority_queue), 0)
  assert_true(PriorityQueue::is_empty(priority_queue))
}

// Test 4: 高效环形缓冲区操作测试
test "efficient circular buffer operations" {
  // 创建环形缓冲区
  let buffer = CircularBuffer::new(5)
  
  // 测试初始状态
  assert_true(CircularBuffer::is_empty(buffer))
  assert_false(CircularBuffer::is_full(buffer))
  assert_eq(CircularBuffer::size(buffer), 0)
  
  // 添加元素
  assert_true(CircularBuffer::push(buffer, 1))
  assert_true(CircularBuffer::push(buffer, 2))
  assert_true(CircularBuffer::push(buffer, 3))
  
  // 测试缓冲区状态
  assert_false(CircularBuffer::is_empty(buffer))
  assert_false(CircularBuffer::is_full(buffer))
  assert_eq(CircularBuffer::size(buffer), 3)
  
  // 测试元素访问
  assert_eq(CircularBuffer::get(buffer, 0), Some(1))
  assert_eq(CircularBuffer::get(buffer, 1), Some(2))
  assert_eq(CircularBuffer::get(buffer, 2), Some(3))
  assert_eq(CircularBuffer::get(buffer, 3), None) // 索引超出范围
  
  // 填满缓冲区
  assert_true(CircularBuffer::push(buffer, 4))
  assert_true(CircularBuffer::push(buffer, 5))
  
  // 缓冲区现在应该满了
  assert_false(CircularBuffer::is_empty(buffer))
  assert_true(CircularBuffer::is_full(buffer))
  assert_eq(CircularBuffer::size(buffer), 5)
  
  // 尝试添加更多元素（应该失败）
  assert_false(CircularBuffer::push(buffer, 6))
  
  // 测试覆盖模式下的添加
  CircularBuffer::push_overwrite(buffer, 6)
  assert_true(CircularBuffer::is_full(buffer))
  assert_eq(CircularBuffer::size(buffer), 5)
  
  // 最旧的元素应该被覆盖
  assert_eq(CircularBuffer::get(buffer, 0), Some(2))
  assert_eq(CircularBuffer::get(buffer, 4), Some(6))
  
  // 测试元素移除
  let removed = CircularBuffer::pop(buffer)
  assert_eq(removed, Some(2))
  assert_false(CircularBuffer::is_full(buffer))
  assert_eq(CircularBuffer::size(buffer), 4)
  
  // 测试迭代器
  let mut sum = 0
  for item in CircularBuffer::iter(buffer) {
    sum = sum + item
  }
  assert_eq(sum, 20) // 3 + 4 + 5 + 6
}

// Test 5: 高效哈希表操作测试
test "efficient hash table operations" {
  // 创建哈希表
  let hash_table = HashTable::new(10)
  
  // 测试初始状态
  assert_eq(HashTable::size(hash_table), 0)
  assert_true(HashTable::is_empty(hash_table))
  
  // 添加键值对
  assert_true(HashTable::insert(hash_table, "key1", "value1"))
  assert_true(HashTable::insert(hash_table, "key2", "value2"))
  assert_true(HashTable::insert(hash_table, "key3", "value3"))
  
  // 测试表大小
  assert_eq(HashTable::size(hash_table), 3)
  assert_false(HashTable::is_empty(hash_table))
  
  // 测试值获取
  assert_eq(HashTable::get(hash_table, "key1"), Some("value1"))
  assert_eq(HashTable::get(hash_table, "key2"), Some("value2"))
  assert_eq(HashTable::get(hash_table, "key3"), Some("value3"))
  assert_eq(HashTable::get(hash_table, "nonexistent"), None)
  
  // 测试键存在性检查
  assert_true(HashTable::contains_key(hash_table, "key1"))
  assert_true(HashTable::contains_key(hash_table, "key2"))
  assert_false(HashTable::contains_key(hash_table, "nonexistent"))
  
  // 测试值更新
  assert_true(HashTable::insert(hash_table, "key1", "new_value1"))
  assert_eq(HashTable::get(hash_table, "key1"), Some("new_value1"))
  assert_eq(HashTable::size(hash_table), 3) // 大小不变，只是更新了值
  
  // 测试键值对删除
  let removed_value = HashTable::remove(hash_table, "key2")
  assert_eq(removed_value, Some("value2"))
  assert_eq(HashTable::size(hash_table), 2)
  assert_false(HashTable::contains_key(hash_table, "key2"))
  
  // 测试删除不存在的键
  let non_existent_removal = HashTable::remove(hash_table, "nonexistent")
  assert_eq(non_existent_removal, None)
  assert_eq(HashTable::size(hash_table), 2) // 大小不变
  
  // 测试获取所有键
  let all_keys = HashTable::keys(hash_table)
  assert_eq(all_keys.length(), 2)
  assert_true(all_keys.contains("key1"))
  assert_true(all_keys.contains("key3"))
  
  // 测试获取所有值
  let all_values = HashTable::values(hash_table)
  assert_eq(all_values.length(), 2)
  assert_true(all_values.contains("new_value1"))
  assert_true(all_values.contains("value3"))
  
  // 测试清空表
  HashTable::clear(hash_table)
  assert_eq(HashTable::size(hash_table), 0)
  assert_true(HashTable::is_empty(hash_table))
  assert_eq(HashTable::get(hash_table, "key1"), None)
}

// Test 6: 高效树结构操作测试
test "efficient tree structure operations" {
  // 创建二叉搜索树
  let tree = BinarySearchTree::new()
  
  // 测试初始状态
  assert_true(BinarySearchTree::is_empty(tree))
  assert_eq(BinarySearchTree::size(tree), 0)
  
  // 插入节点
  BinarySearchTree::insert(tree, 50)
  BinarySearchTree::insert(tree, 30)
  BinarySearchTree::insert(tree, 70)
  BinarySearchTree::insert(tree, 20)
  BinarySearchTree::insert(tree, 40)
  BinarySearchTree::insert(tree, 60)
  BinarySearchTree::insert(tree, 80)
  
  // 测试树大小
  assert_false(BinarySearchTree::is_empty(tree))
  assert_eq(BinarySearchTree::size(tree), 7)
  
  // 测试查找
  assert_true(BinarySearchTree::contains(tree, 50))
  assert_true(BinarySearchTree::contains(tree, 20))
  assert_true(BinarySearchTree::contains(tree, 80))
  assert_false(BinarySearchTree::contains(tree, 90))
  
  // 测试最小值和最大值
  assert_eq(BinarySearchTree::min(tree), Some(20))
  assert_eq(BinarySearchTree::max(tree), Some(80))
  
  // 测试前序遍历
  let preorder = BinarySearchTree::preorder_traversal(tree)
  assert_eq(preorder, [50, 30, 20, 40, 70, 60, 80])
  
  // 测试中序遍历
  let inorder = BinarySearchTree::inorder_traversal(tree)
  assert_eq(inorder, [20, 30, 40, 50, 60, 70, 80])
  
  // 测试后序遍历
  let postorder = BinarySearchTree::postorder_traversal(tree)
  assert_eq(postorder, [20, 40, 30, 60, 80, 70, 50])
  
  // 测试层序遍历
  let levelorder = BinarySearchTree::levelorder_traversal(tree)
  assert_eq(levelorder, [50, 30, 70, 20, 40, 60, 80])
  
  // 测试节点删除（叶子节点）
  BinarySearchTree::remove(tree, 20)
  assert_false(BinarySearchTree::contains(tree, 20))
  assert_eq(BinarySearchTree::size(tree), 6)
  
  // 测试节点删除（有一个子节点的节点）
  BinarySearchTree::remove(tree, 30)
  assert_false(BinarySearchTree::contains(tree, 30))
  assert_eq(BinarySearchTree::size(tree), 5)
  
  // 测试节点删除（有两个子节点的节点）
  BinarySearchTree::remove(tree, 50)
  assert_false(BinarySearchTree::contains(tree, 50))
  assert_eq(BinarySearchTree::size(tree), 4)
  
  // 验证树结构仍然正确
  let new_inorder = BinarySearchTree::inorder_traversal(tree)
  assert_eq(new_inorder, [40, 60, 70, 80])
}

// Test 7: 高效图结构操作测试
test "efficient graph structure operations" {
  // 创建有向图
  let graph = DirectedGraph::new(10)
  
  // 测试初始状态
  assert_eq(DirectedGraph::vertex_count(graph), 0)
  assert_eq(DirectedGraph::edge_count(graph), 0)
  
  // 添加顶点
  let v1 = DirectedGraph::add_vertex(graph, "A")
  let v2 = DirectedGraph::add_vertex(graph, "B")
  let v3 = DirectedGraph::add_vertex(graph, "C")
  let v4 = DirectedGraph::add_vertex(graph, "D")
  
  // 测试顶点数量
  assert_eq(DirectedGraph::vertex_count(graph), 4)
  
  // 添加边
  DirectedGraph::add_edge(graph, v1, v2, 5)
  DirectedGraph::add_edge(graph, v1, v3, 3)
  DirectedGraph::add_edge(graph, v2, v3, 2)
  DirectedGraph::add_edge(graph, v2, v4, 6)
  DirectedGraph::add_edge(graph, v3, v4, 7)
  
  // 测试边数量
  assert_eq(DirectedGraph::edge_count(graph), 5)
  
  // 测试顶点邻居
  let v1_neighbors = DirectedGraph::neighbors(graph, v1)
  assert_eq(v1_neighbors.length(), 2)
  assert_true(v1_neighbors.contains(v2))
  assert_true(v1_neighbors.contains(v3))
  
  let v4_neighbors = DirectedGraph::neighbors(graph, v4)
  assert_eq(v4_neighbors.length(), 0) // v4没有出边
  
  // 测试边权重
  assert_eq(DirectedGraph::edge_weight(graph, v1, v2), Some(5))
  assert_eq(DirectedGraph::edge_weight(graph, v1, v3), Some(3))
  assert_eq(DirectedGraph::edge_weight(graph, v4, v1), None) // 不存在的边
  
  // 测试顶点度数
  assert_eq(DirectedGraph::out_degree(graph, v1), 2)
  assert_eq(DirectedGraph::in_degree(graph, v1), 0)
  assert_eq(DirectedGraph::out_degree(graph, v4), 0)
  assert_eq(DirectedGraph::in_degree(graph, v4), 2)
  
  // 测试Dijkstra最短路径算法
  let shortest_path_v1_to_v4 = DirectedGraph::shortest_path(graph, v1, v4)
  match shortest_path_v1_to_v4 {
    Some((distance, path)) => {
      assert_eq(distance, 10) // v1->v3->v4: 3 + 7
      assert_eq(path, [v1, v3, v4])
    }
    None => assert_true(false)
  }
  
  // 测试删除边
  DirectedGraph::remove_edge(graph, v1, v3)
  assert_eq(DirectedGraph::edge_count(graph), 4)
  assert_eq(DirectedGraph::edge_weight(graph, v1, v3), None)
  
  // 重新计算最短路径
  let new_shortest_path = DirectedGraph::shortest_path(graph, v1, v4)
  match new_shortest_path {
    Some((distance, path)) => {
      assert_eq(distance, 11) // v1->v2->v4: 5 + 6
      assert_eq(path, [v1, v2, v4])
    }
    None => assert_true(false)
  }
  
  // 测试删除顶点
  DirectedGraph::remove_vertex(graph, v2)
  assert_eq(DirectedGraph::vertex_count(graph), 3)
  assert_eq(DirectedGraph::edge_count(graph), 1) // 只剩下v3->v4
  
  // 测试拓扑排序
  let topological_order = DirectedGraph::topological_sort(graph)
  match topological_order {
    Some(order) => {
      assert_eq(order.length(), 3)
      // v3应该在v4前面，因为有一条边v3->v4
      let v3_index = order.index_of(v3)
      let v4_index = order.index_of(v4)
      assert_true(v3_index < v4_index)
    }
    None => assert_true(false)
  }
}