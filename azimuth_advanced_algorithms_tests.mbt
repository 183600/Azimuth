// Azimuth 高级算法测试用例
// 专注于高级算法和数据处理

// 测试1: 时间序列分析算法
test "时间序列分析算法" {
  // 模拟移动平均算法
  let calculate_simple_moving_average = fn(data: Array<Float>, window_size: Int) {
    if data.length() < window_size || window_size <= 0 {
      return []
    }
    
    let mut sma = []
    let mut window_sum = 0.0
    
    // 计算第一个窗口
    for i in 0..window_size {
      window_sum = window_sum + data[i]
    }
    sma = sma.push(window_sum / window_size.to_float())
    
    // 滑动窗口
    for i in window_size..data.length() {
      window_sum = window_sum - data[i - window_size] + data[i]
      sma = sma.push(window_sum / window_size.to_float())
    }
    
    sma
  }
  
  // 模拟指数移动平均算法
  let calculate_exponential_moving_average = fn(data: Array<Float>, alpha: Float) {
    if data.length() == 0 || alpha <= 0.0 || alpha > 1.0 {
      return []
    }
    
    let mut ema = []
    ema = ema.push(data[0])  // 第一个EMA值等于第一个数据点
    
    for i in 1..data.length() {
      let ema_value = alpha * data[i] + (1.0 - alpha) * ema[i - 1]
      ema = ema.push(ema_value)
    }
    
    ema
  }
  
  // 模拟异常值检测（Z-score方法）
  let detect_anomalies_zscore = fn(data: Array<Float>, threshold: Float) {
    if data.length() < 3 {
      return []
    }
    
    // 计算平均值
    let mut sum = 0.0
    for value in data {
      sum = sum + value
    }
    let mean = sum / data.length().to_float()
    
    // 计算标准差
    let mut variance_sum = 0.0
    for value in data {
      let diff = value - mean
      variance_sum = variance_sum + diff * diff
    }
    let variance = variance_sum / data.length().to_float()
    let std_dev = if variance >= 0.0 { variance.sqrt() } else { 0.0 }
    
    // 检测异常值
    let mut anomalies = []
    for i in 0..data.length() {
      let value = data[i]
      let z_score = if std_dev > 0.0 { (value - mean).abs() / std_dev } else { 0.0 }
      
      if z_score > threshold {
        anomalies = anomalies.push((i, value, z_score))
      }
    }
    
    anomalies
  }
  
  // 测试时间序列分析
  let time_series_data = [10.0, 12.0, 15.0, 14.0, 18.0, 20.0, 22.0, 19.0, 25.0, 30.0, 28.0, 35.0, 40.0, 38.0, 45.0]
  
  // 测试移动平均
  let sma_3 = calculate_simple_moving_average(time_series_data, 3)
  let sma_5 = calculate_simple_moving_average(time_series_data, 5)
  
  assert_eq(sma_3.length(), 13)  // 15 - 3 + 1
  assert_eq(sma_5.length(), 11)  // 15 - 5 + 1
  
  // 验证第一个SMA值
  assert_eq(sma_3[0], (10.0 + 12.0 + 15.0) / 3.0)
  assert_eq(sma_5[0], (10.0 + 12.0 + 15.0 + 14.0 + 18.0) / 5.0)
  
  // 测试指数移动平均
  let ema = calculate_exponential_moving_average(time_series_data, 0.3)
  assert_eq(ema.length(), 15)
  assert_eq(ema[0], 10.0)  // 第一个值等于原始数据
  
  // 验证第二个EMA值
  let expected_ema1 = 0.3 * 12.0 + 0.7 * 10.0
  assert_eq(ema[1], expected_ema1)
  
  // 测试异常值检测
  let data_with_anomalies = [10.0, 12.0, 15.0, 14.0, 18.0, 20.0, 22.0, 19.0, 25.0, 30.0, 28.0, 35.0, 40.0, 38.0, 100.0]  // 最后一个值是异常值
  let anomalies = detect_anomalies_zscore(data_with_anomalies, 2.0)
  
  assert_true(anomalies.length() >= 1)  // 应该检测到至少一个异常值
  assert_eq(anomalies[0].0, 14)  // 异常值在索引14
  assert_eq(anomalies[0].1, 100.0)  // 异常值是100.0
  
  // 测试趋势分析
  let calculate_trend = fn(data: Array<Float>) {
    if data.length() < 2 {
      return "insufficient_data"
    }
    
    let mut increasing_count = 0
    let mut decreasing_count = 0
    
    for i in 1..data.length() {
      if data[i] > data[i - 1] {
        increasing_count = increasing_count + 1
      } else if data[i] < data[i - 1] {
        decreasing_count = decreasing_count + 1
      }
    }
    
    if increasing_count > decreasing_count * 1.5 {
      "increasing"
    } else if decreasing_count > increasing_count * 1.5 {
      "decreasing"
    } else {
      "stable"
    }
  }
  
  let trend = calculate_trend(time_series_data)
  assert_eq(trend, "increasing")  // 数据总体呈上升趋势
}

// 测试2: 聚类算法
test "聚类算法" {
  // 模拟K-means聚类算法
  let kmeans_clustering = fn(data: Array<{ x: Float, y: Float }>, k: Int, max_iterations: Int) {
    if data.length() < k || k <= 0 {
      return []
    }
    
    // 初始化聚类中心（选择前k个点）
    let mut centroids = []
    for i in 0..k {
      centroids = centroids.push(data[i])
    }
    
    let mut assignments = []
    for i in 0..data.length() {
      assignments = assignments.push(0)  // 初始分配到第一个聚类
    }
    
    let mut iteration = 0
    let mut converged = false
    
    while iteration < max_iterations && not(converged) {
      // 分配点到最近的聚类中心
      let mut new_assignments = []
      
      for point in data {
        let mut min_distance = Float::max_value()
        let mut closest_centroid = 0
        
        for i in 0..centroids.length() {
          let centroid = centroids[i]
          let distance = ((point.x - centroid.x) * (point.x - centroid.x) + (point.y - centroid.y) * (point.y - centroid.y)).sqrt()
          
          if distance < min_distance {
            min_distance = distance
            closest_centroid = i
          }
        }
        
        new_assignments = new_assignments.push(closest_centroid)
      }
      
      // 检查是否收敛
      converged = true
      for i in 0..assignments.length() {
        if assignments[i] != new_assignments[i] {
          converged = false
          break
        }
      }
      
      assignments = new_assignments
      
      // 更新聚类中心
      let mut new_centroids = []
      
      for i in 0..k {
        let mut cluster_points = []
        let mut sum_x = 0.0
        let mut sum_y = 0.0
        let mut count = 0
        
        for j in 0..data.length() {
          if assignments[j] == i {
            sum_x = sum_x + data[j].x
            sum_y = sum_y + data[j].y
            count = count + 1
          }
        }
        
        if count > 0 {
          new_centroids = new_centroids.push({ x: sum_x / count.to_float(), y: sum_y / count.to_float() })
        } else {
          new_centroids = new_centroids.push(centroids[i])  // 保持原中心
        }
      }
      
      centroids = new_centroids
      iteration = iteration + 1
    }
    
    // 返回聚类结果
    let mut clusters = []
    for i in 0..k {
      let mut cluster_points = []
      for j in 0..data.length() {
        if assignments[j] == i {
          cluster_points = cluster_points.push(data[j])
        }
      }
      clusters = clusters.push({
        centroid: centroids[i],
        points: cluster_points
      })
    }
    
    clusters
  }
  
  // 测试K-means聚类
  let data_points = [
    { x: 1.0, y: 1.0 },
    { x: 1.5, y: 2.0 },
    { x: 3.0, y: 4.0 },
    { x: 5.0, y: 7.0 },
    { x: 3.5, y: 5.0 },
    { x: 4.5, y: 5.0 },
    { x: 3.5, y: 4.5 },
    { x: 2.0, y: 2.0 },
    { x: 1.0, y: 2.0 },
    { x: 5.0, y: 6.0 }
  ]
  
  let clusters = kmeans_clustering(data_points, 3, 10)
  
  assert_eq(clusters.length(), 3)
  
  // 验证每个聚类都有点
  for cluster in clusters {
    assert_true(cluster.points.length() > 0)
  }
  
  // 验证所有点都被分配
  let mut total_points = 0
  for cluster in clusters {
    total_points = total_points + cluster.points.length()
  }
  assert_eq(total_points, data_points.length())
  
  // 测试层次聚类
  let hierarchical_clustering = fn(data: Array<{ x: Float, y: Float }>, linkage: String) {
    if data.length() < 2 {
      return []
    }
    
    // 初始化：每个点是一个聚类
    let mut clusters = []
    for point in data {
      clusters = clusters.push({
        points: [point],
        centroid: point
      })
    }
    
    while clusters.length() > 1 {
      // 找到最近的两个聚类
      let mut min_distance = Float::max_value()
      let mut merge_i = 0
      let mut merge_j = 1
      
      for i in 0..clusters.length() {
        for j in i + 1..clusters.length() {
          let distance = ((clusters[i].centroid.x - clusters[j].centroid.x) * (clusters[i].centroid.x - clusters[j].centroid.x) + 
                         (clusters[i].centroid.y - clusters[j].centroid.y) * (clusters[i].centroid.y - clusters[j].centroid.y)).sqrt()
          
          if distance < min_distance {
            min_distance = distance
            merge_i = i
            merge_j = j
          }
        }
      }
      
      // 合并聚类
      let merged_points = clusters[merge_i].points + clusters[merge_j].points
      
      // 计算新的聚类中心
      let mut sum_x = 0.0
      let mut sum_y = 0.0
      for point in merged_points {
        sum_x = sum_x + point.x
        sum_y = sum_y + point.y
      }
      
      let new_centroid = {
        x: sum_x / merged_points.length().to_float(),
        y: sum_y / merged_points.length().to_float()
      }
      
      let new_cluster = {
        points: merged_points,
        centroid: new_centroid
      }
      
      // 更新聚类列表
      let mut new_clusters = []
      for i in 0..clusters.length() {
        if i != merge_i && i != merge_j {
          new_clusters = new_clusters.push(clusters[i])
        }
      }
      new_clusters = new_clusters.push(new_cluster)
      
      clusters = new_clusters
      
      // 如果只需要两个聚类，可以提前终止
      if clusters.length() == 2 {
        break
      }
    }
    
    clusters
  }
  
  // 测试层次聚类
  let hierarchical_clusters = hierarchical_clustering(data_points, "average")
  
  assert_eq(hierarchical_clusters.length(), 2)
  
  // 验证所有点都被分配
  let mut hierarchical_total_points = 0
  for cluster in hierarchical_clusters {
    hierarchical_total_points = hierarchical_total_points + cluster.points.length()
  }
  assert_eq(hierarchical_total_points, data_points.length())
}

// 测试3: 推荐算法
test "推荐算法" {
  // 模拟基于内容的推荐
  let content_based_recommendation = fn(user_profile: { String: Float }, items: Array<{ id: String, features: { String: Float } }>, num_recommendations: Int) {
    let mut scores = []
    
    for item in items {
      let mut score = 0.0
      let mut total_weight = 0.0
      
      for (feature, weight) in user_profile {
        match item.features[feature] {
          Some(item_value) => {
            score = score + weight * item_value
            total_weight = total_weight + weight.abs()
          }
          None => ()
        }
      }
      
      if total_weight > 0.0 {
        score = score / total_weight
      }
      
      scores = scores.push((item.id, score))
    }
    
    // 按分数排序（简化：选择前N个）
    let mut sorted_scores = scores
    let mut i = 0
    while i < sorted_scores.length() - 1 {
      let mut j = i + 1
      while j < sorted_scores.length() {
        if sorted_scores[i].1 < sorted_scores[j].1 {
          let temp = sorted_scores[i]
          sorted_scores[i] = sorted_scores[j]
          sorted_scores[j] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    
    // 返回前N个推荐
    let mut recommendations = []
    let count = if num_recommendations < sorted_scores.length() { num_recommendations } else { sorted_scores.length() }
    
    for i in 0..count {
      recommendations = recommendations.push(sorted_scores[i].0)
    }
    
    recommendations
  }
  
  // 模拟协同过滤推荐
  let collaborative_filtering = fn(user_item_ratings: { String: { String: Float } }, target_user: String, num_recommendations: Int) {
    match user_item_ratings[target_user] {
      Some(target_ratings) => {
        let mut similarities = []
        
        // 计算与其他用户的相似度
        for (user, ratings) in user_item_ratings {
          if user != target_user {
            let mut similarity = 0.0
            let mut common_items = 0
            
            for (item, rating) in target_ratings {
              match ratings[item] {
                Some(other_rating) => {
                  similarity = similarity + rating * other_rating
                  common_items = common_items + 1
                }
                None => ()
              }
            }
            
            if common_items > 0 {
              similarities = similarities.push((user, similarity / common_items.to_float()))
            }
          }
        }
        
        // 找到相似用户并预测评分
        let mut predictions = {}
        
        for (user, similarity) in similarities {
          if similarity > 0.0 {
            match user_item_ratings[user] {
              Some(user_ratings) => {
                for (item, rating) in user_ratings {
                  match target_ratings[item] {
                    Some(_) => ()  // 用户已评分，跳过
                    None => {
                      let current_score = 0.0
                      match predictions[item] {
                        Some(score) => current_score = score
                        None => ()
                      }
                      predictions[item] = current_score + similarity * rating
                    }
                  }
                }
              }
              None => ()
            }
          }
        }
        
        // 按预测评分排序
        let mut sorted_predictions = []
        for (item, score) in predictions {
          sorted_predictions = sorted_predictions.push((item, score))
        }
        
        // 简化排序
        let mut i = 0
        while i < sorted_predictions.length() - 1 {
          let mut j = i + 1
          while j < sorted_predictions.length() {
            if sorted_predictions[i].1 < sorted_predictions[j].1 {
              let temp = sorted_predictions[i]
              sorted_predictions[i] = sorted_predictions[j]
              sorted_predictions[j] = temp
            }
            j = j + 1
          }
          i = i + 1
        }
        
        // 返回前N个推荐
        let mut recommendations = []
        let count = if num_recommendations < sorted_predictions.length() { num_recommendations } else { sorted_predictions.length() }
        
        for i in 0..count {
          recommendations = recommendations.push(sorted_predictions[i].0)
        }
        
        recommendations
      }
      None => []
    }
  }
  
  // 测试基于内容的推荐
  let user_profile = {
    "action": 0.8,
    "comedy": 0.6,
    "drama": 0.4,
    "sci-fi": 0.9,
    "romance": 0.2
  }
  
  let items = [
    { id: "movie1", features: { "action": 0.9, "sci-fi": 0.8, "thriller": 0.7 } },
    { id: "movie2", features: { "comedy": 0.9, "romance": 0.8 } },
    { id: "movie3", features: { "drama": 0.9, "romance": 0.7 } },
    { id: "movie4", features: { "sci-fi": 0.9, "action": 0.8, "adventure": 0.7 } },
    { id: "movie5", features: { "comedy": 0.8, "action": 0.6 } }
  ]
  
  let content_recommendations = content_based_recommendation(user_profile, items, 3)
  
  assert_eq(content_recommendations.length(), 3)
  assert_true(content_recommendations.contains("movie1"))  // 高匹配度
  assert_true(content_recommendations.contains("movie4"))  // 高匹配度
  
  // 测试协同过滤推荐
  let user_item_ratings = {
    "user1": {
      "movie1": 5.0,
      "movie2": 3.0,
      "movie3": 4.0
    },
    "user2": {
      "movie1": 4.0,
      "movie2": 2.0,
      "movie4": 5.0,
      "movie5": 3.0
    },
    "user3": {
      "movie2": 4.0,
      "movie3": 5.0,
      "movie4": 4.0,
      "movie5": 5.0
    }
  }
  
  let cf_recommendations = collaborative_filtering(user_item_ratings, "user1", 2)
  
  assert_eq(cf_recommendations.length(), 2)
  // user1已经评分了movie1, movie2, movie3，所以应该推荐movie4和movie5
  
  // 测试混合推荐
  let hybrid_recommendation = fn(content_recs: Array<String>, cf_recs: Array<String>, content_weight: Float) {
    let mut hybrid_scores = {}
    
    // 内容推荐得分
    for i in 0..content_recs.length() {
      let score = (content_recs.length() - i).to_float() / content_recs.length().to_float()
      hybrid_scores[content_recs[i]] = score * content_weight
    }
    
    // 协同过滤推荐得分
    for i in 0..cf_recs.length() {
      let score = (cf_recs.length() - i).to_float() / cf_recs.length().to_float()
      let current_score = 0.0
      match hybrid_scores[cf_recs[i]] {
        Some(s) => current_score = s
        None => ()
      }
      hybrid_scores[cf_recs[i]] = current_score + score * (1.0 - content_weight)
    }
    
    // 按得分排序
    let mut sorted_recs = []
    for (item, score) in hybrid_scores {
      sorted_recs = sorted_recs.push((item, score))
    }
    
    // 简化排序
    let mut i = 0
    while i < sorted_recs.length() - 1 {
      let mut j = i + 1
      while j < sorted_recs.length() {
        if sorted_recs[i].1 < sorted_recs[j].1 {
          let temp = sorted_recs[i]
          sorted_recs[i] = sorted_recs[j]
          sorted_recs[j] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    
    // 返回排序后的项目
    let mut recommendations = []
    for (item, _) in sorted_recs {
      recommendations = recommendations.push(item)
    }
    
    recommendations
  }
  
  let hybrid_recs = hybrid_recommendation(content_recommendations, cf_recommendations, 0.6)
  
  assert_true(hybrid_recs.length() > 0)
  // 混合推荐应该包含两种推荐方法的结果，但按加权得分排序
}

// 测试4: 图算法
test "图算法" {
  // 模拟图结构
  type Graph = {
    nodes: Array<String>,
    edges: Array<{ from: String, to: String, weight: Float }>
  }
  
  // 创建图
  let create_graph = fn() {
    {
      nodes: [],
      edges: []
    }
  }
  
  let add_node = fn(graph: Graph, node: String) {
    if not(graph.nodes.contains(node)) {
      {
        nodes: graph.nodes.push(node),
        edges: graph.edges
      }
    } else {
      graph
    }
  }
  
  let add_edge = fn(graph: Graph, from: String, to: String, weight: Float) {
    {
      nodes: graph.nodes,
      edges: graph.edges.push({ from, to, weight })
    }
  }
  
  // Dijkstra最短路径算法
  let dijkstra_shortest_path = fn(graph: Graph, start: String, end: String) {
    if not(graph.nodes.contains(start)) || not(graph.nodes.contains(end)) {
      return None
    }
    
    // 初始化距离和前驱节点
    let mut distances = {}
    let mut previous = {}
    let mut unvisited = []
    
    for node in graph.nodes {
      distances[node] = if node == start { 0.0 } else { Float::max_value() }
      unvisited = unvisited.push(node)
    }
    
    while unvisited.length() > 0 {
      // 找到未访问节点中距离最小的
      let mut min_distance = Float::max_value()
      let mut current = ""
      let mut current_index = -1
      
      for i in 0..unvisited.length() {
        let node = unvisited[i]
        match distances[node] {
          Some(dist) => {
            if dist < min_distance {
              min_distance = dist
              current = node
              current_index = i
            }
          }
          None => ()
        }
      }
      
      if current == "" || current == end {
        break
      }
      
      // 从未访问中移除当前节点
      let mut new_unvisited = []
      for i in 0..unvisited.length() {
        if i != current_index {
          new_unvisited = new_unvisited.push(unvisited[i])
        }
      }
      unvisited = new_unvisited
      
      // 更新邻居的距离
      for edge in graph.edges {
        if edge.from == current {
          match distances[current] {
            Some(current_dist) => {
              let alt = current_dist + edge.weight
              match distances[edge.to] {
                Some(neighbor_dist) => {
                  if alt < neighbor_dist {
                    distances[edge.to] = alt
                    previous[edge.to] = current
                  }
                }
                None => ()
              }
            }
            None => ()
          }
        }
      }
    }
    
    // 构建路径
    if not(distances.contains_key(end)) {
      return None
    }
    
    let mut path = []
    let mut current = end
    
    while current != "" {
      path = path.push(current)
      match previous[current] {
        Some(prev) => current = prev
        None => {
          if current != start {
            return None  // 路径不完整
          } else {
            current = ""
          }
        }
      }
    }
    
    // 反转路径
    let mut reversed_path = []
    for i in path.length() - 1 .. 0 {
      reversed_path = reversed_path.push(path[i])
    }
    
    match distances[end] {
      Some(distance) => Some((reversed_path, distance))
      None => None
    }
  }
  
  // 测试图算法
  let mut graph = create_graph()
  
  // 添加节点
  graph = add_node(graph, "A")
  graph = add_node(graph, "B")
  graph = add_node(graph, "C")
  graph = add_node(graph, "D")
  graph = add_node(graph, "E")
  
  // 添加边
  graph = add_edge(graph, "A", "B", 6.0)
  graph = add_edge(graph, "A", "D", 1.0)
  graph = add_edge(graph, "B", "D", 2.0)
  graph = add_edge(graph, "B", "E", 2.0)
  graph = add_edge(graph, "B", "C", 5.0)
  graph = add_edge(graph, "D", "E", 1.0)
  graph = add_edge(graph, "E", "C", 5.0)
  
  // 测试最短路径
  let path_a_c = dijkstra_shortest_path(graph, "A", "C")
  
  assert_true(path_a_c.is_some())
  
  match path_a_c {
    Some((path, distance)) => {
      assert_eq(path, ["A", "D", "E", "C"])  // A->D->E->C是最短路径
      assert_eq(distance, 1.0 + 1.0 + 5.0)  // 距离应该是7.0
    }
    None => assert_true(false)
  }
  
  let path_a_e = dijkstra_shortest_path(graph, "A", "E")
  
  match path_a_e {
    Some((path, distance)) => {
      assert_eq(path, ["A", "D", "E"])  // A->D->E是最短路径
      assert_eq(distance, 1.0 + 1.0)  // 距离应该是2.0
    }
    None => assert_true(false)
  }
  
  // 测试不存在的路径
  let path_a_x = dijkstra_shortest_path(graph, "A", "X")
  assert_true(path_a_x.is_none())
  
  // 测试连通分量
  let find_connected_components = fn(graph: Graph) {
    let mut visited = {}
    let mut components = []
    
    for node in graph.nodes {
      if not(visited.contains_key(node)) {
        let mut component = []
        let mut stack = [node]
        
        while stack.length() > 0 {
          let current = stack[stack.length() - 1]
          stack = stack.slice(0, stack.length() - 1)
          
          if not(visited.contains_key(current)) {
            visited[current] = true
            component = component.push(current)
            
            // 添加邻居到栈中
            for edge in graph.edges {
              if edge.from == current && not(visited.contains_key(edge.to)) {
                stack = stack.push(edge.to)
              } else if edge.to == current && not(visited.contains_key(edge.from)) {
                stack = stack.push(edge.from)
              }
            }
          }
        }
        
        components = components.push(component)
      }
    }
    
    components
  }
  
  // 创建一个有多个连通分量的图
  let mut disconnected_graph = create_graph()
  
  // 第一个连通分量
  disconnected_graph = add_node(disconnected_graph, "X")
  disconnected_graph = add_node(disconnected_graph, "Y")
  disconnected_graph = add_node(disconnected_graph, "Z")
  disconnected_graph = add_edge(disconnected_graph, "X", "Y", 1.0)
  disconnected_graph = add_edge(disconnected_graph, "Y", "Z", 1.0)
  
  // 第二个连通分量
  disconnected_graph = add_node(disconnected_graph, "P")
  disconnected_graph = add_node(disconnected_graph, "Q")
  disconnected_graph = add_edge(disconnected_graph, "P", "Q", 1.0)
  
  // 第三个连通分量（孤立节点）
  disconnected_graph = add_node(disconnected_graph, "R")
  
  let components = find_connected_components(disconnected_graph)
  
  assert_eq(components.length(), 3)
  
  // 验证每个分量
  let component_sizes = components.map(fn(comp) { comp.length() })
  assert_true(component_sizes.contains(3))  // X-Y-Z分量
  assert_true(component_sizes.contains(2))  // P-Q分量
  assert_true(component_sizes.contains(1))  // R分量
}

// 测试5: 自然语言处理算法
test "自然语言处理算法" {
  // 模拟文本预处理
  let tokenize = fn(text: String) {
    let mut tokens = []
    let mut current_token = ""
    
    for char in text.to_char_array() {
      if char == ' ' || char == '\t' || char == '\n' || char == '\r' {
        if current_token != "" {
          tokens = tokens.push(current_token.to_lowercase())
          current_token = ""
        }
      } else if (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9') {
        current_token = current_token + char.to_string()
      } else {
        // 标点符号，结束当前token
        if current_token != "" {
          tokens = tokens.push(current_token.to_lowercase())
          current_token = ""
        }
      }
    }
    
    if current_token != "" {
      tokens = tokens.push(current_token.to_lowercase())
    }
    
    tokens
  }
  
  let remove_stop_words = fn(tokens: Array<String>, stop_words: Array<String>) {
    let mut filtered = []
    
    for token in tokens {
      if not(stop_words.contains(token)) {
        filtered = filtered.push(token)
      }
    }
    
    filtered
  }
  
  let calculate_tf_idf = fn(documents: Array<Array<String>>) {
    let mut tf = []
    let mut df = {}  // 文档频率
    let mut total_docs = documents.length().to_float()
    
    // 计算词频(TF)和文档频率(DF)
    for doc in documents {
      let mut doc_tf = {}
      let mut doc_tokens = {}
      
      for token in doc {
        let count = 0
        match doc_tokens[token] {
          Some(c) => count = c
          None => ()
        }
        doc_tokens[token] = count + 1
        
        // 更新文档频率
        if not(df.contains_key(token)) {
          df[token] = 0
        }
        df[token] = df[token] + 1
      }
      
      // 计算归一化TF
      let token_count = doc.length().to_float()
      let mut normalized_tf = {}
      
      for (token, count) in doc_tokens {
        normalized_tf[token] = count.to_float() / token_count
      }
      
      tf = tf.push(normalized_tf)
    }
    
    // 计算TF-IDF
    let mut tf_idf = []
    
    for doc_tf in tf {
      let mut doc_tfidf = {}
      
      for (token, tf_value) in doc_tf {
        let idf = (total_docs / df[token].to_float()).log()
        doc_tfidf[token] = tf_value * idf
      }
      
      tf_idf = tf_idf.push(doc_tfidf)
    }
    
    tf_idf
  }
  
  // 测试文本预处理
  let text = "The quick brown fox jumps over the lazy dog. The dog was not amused."
  let tokens = tokenize(text)
  
  assert_eq(tokens.length(), 11)
  assert_true(tokens.contains("quick"))
  assert_true(tokens.contains("brown"))
  assert_true(tokens.contains("fox"))
  
  // 测试停用词移除
  let stop_words = ["the", "over", "was", "not"]
  let filtered_tokens = remove_stop_words(tokens, stop_words)
  
  assert_eq(filtered_tokens.length(), 7)
  assert_false(filtered_tokens.contains("the"))
  assert_false(filtered_tokens.contains("over"))
  assert_true(filtered_tokens.contains("quick"))
  assert_true(filtered_tokens.contains("brown"))
  
  // 测试TF-IDF计算
  let documents = [
    tokenize("The cat sat on the mat"),
    tokenize("The dog sat on the log"),
    tokenize("Cats and dogs are pets"),
    tokenize("The mat is soft and warm")
  ]
  
  let tfidf = calculate_tf_idf(documents)
  
  assert_eq(tfidf.length(), 4)
  
  // 验证第一个文档的TF-IDF
  let first_doc_tfidf = tfidf[0]
  assert_true(first_doc_tfidf.contains_key("cat"))
  assert_true(first_doc_tfidf.contains_key("mat"))
  assert_true(first_doc_tfidf.contains_key("sat"))
  
  // 测试文本相似度计算
  let calculate_cosine_similarity = fn(tfidf1: { String: Float }, tfidf2: { String: Float }) {
    let mut dot_product = 0.0
    let mut norm1 = 0.0
    let mut norm2 = 0.0
    
    // 计算点积
    for (term, weight1) in tfidf1 {
      match tfidf2[term] {
        Some(weight2) => {
          dot_product = dot_product + weight1 * weight2
        }
        None => ()
      }
      norm1 = norm1 + weight1 * weight1
    }
    
    for (_, weight2) in tfidf2 {
      norm2 = norm2 + weight2 * weight2
    }
    
    if norm1 > 0.0 && norm2 > 0.0 {
      dot_product / (norm1.sqrt() * norm2.sqrt())
    } else {
      0.0
    }
  }
  
  // 测试文档相似度
  let similarity_0_1 = calculate_cosine_similarity(tfidf[0], tfidf[1])  // "cat mat" vs "dog log"
  let similarity_0_3 = calculate_cosine_similarity(tfidf[0], tfidf[3])  // "cat mat" vs "mat warm"
  
  assert_true(similarity_0_3 > similarity_0_1)  // 文档0和3应该更相似（都包含"mat"）
  
  // 测试文本摘要（简化版）
  let extractive_summary = fn(document: Array<String>, num_sentences: Int) {
    // 简化：基于句子长度和关键词频率选择句子
    if document.length() <= num_sentences {
      return document
    }
    
    // 计算词频
    let mut word_freq = {}
    for word in document {
      let count = 0
      match word_freq[word] {
        Some(c) => count = c
        None => ()
      }
      word_freq[word] = count + 1
    }
    
    // 计算句子得分
    let mut sentence_scores = []
    for i in 0..document.length() {
      let mut score = 0.0
      let words = tokenize(document[i])
      
      for word in words {
        match word_freq[word] {
          Some(freq) => {
            score = score + freq.to_float()
          }
          None => ()
        }
      }
      
      sentence_scores = sentence_scores.push((i, score))
    }
    
    // 按得分排序
    let mut i = 0
    while i < sentence_scores.length() - 1 {
      let mut j = i + 1
      while j < sentence_scores.length() {
        if sentence_scores[i].1 < sentence_scores[j].1 {
          let temp = sentence_scores[i]
          sentence_scores[i] = sentence_scores[j]
          sentence_scores[j] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    
    // 选择前N个句子，并按原始顺序排列
    let mut selected_indices = []
    for i in 0..num_sentences {
      selected_indices = selected_indices.push(sentence_scores[i].0)
    }
    
    // 按原始顺序排序
    let mut i = 0
    while i < selected_indices.length() - 1 {
      let mut j = i + 1
      while j < selected_indices.length() {
        if selected_indices[i] > selected_indices[j] {
          let temp = selected_indices[i]
          selected_indices[i] = selected_indices[j]
          selected_indices[j] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    
    // 生成摘要
    let mut summary = []
    for index in selected_indices {
      summary = summary.push(document[index])
    }
    
    summary
  }
  
  // 测试文本摘要
  let sentences = [
    "The cat sat on the mat.",
    "The mat was comfortable and warm.",
    "A dog watched the cat from afar.",
    "The cat seemed unaware of the dog.",
    "Suddenly, the dog barked loudly.",
    "The cat was startled and ran away."
  ]
  
  let summary = extractive_summary(sentences, 3)
  
  assert_eq(summary.length(), 3)
  // 摘要应该包含原始文档中的3个句子，按原始顺序排列
}