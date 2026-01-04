// Azimuth 数据分析高质量测试用例
// 专注于数据分析、统计计算和数据挖掘

// 测试1: 时间序列数据分析
test "时间序列数据分析测试" {
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metadata: Map[String, String]
  }
  
  // 定义时间序列
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    frequency: String,  // "1m", "5m", "1h", "1d"
    unit: String
  }
  
  // 定义时间序列分析结果
  type TimeSeriesAnalysis = {
    trend: String,  // "increasing", "decreasing", "stable"
    seasonality: Bool,
    outliers: Array[Int],  // 异常点索引
    statistics: TimeSeriesStatistics
  }
  
  // 定义时间序列统计信息
  type TimeSeriesStatistics = {
    mean: Float,
    median: Float,
    std_dev: Float,
    min: Float,
    max: Float,
    percentiles: Map[String, Float]
  }
  
  // 创建时间序列点
  let create_time_series_point = fn(timestamp: Int, value: Float) {
    {
      timestamp,
      value,
      metadata: Map::new()
    }
  }
  
  // 创建时间序列
  let create_time_series = fn(name: String, points: Array[TimeSeriesPoint], frequency: String, unit: String) {
    {
      name,
      points,
      frequency,
      unit
    }
  }
  
  // 计算平均值
  let calculate_mean = fn(values: Array[Float]) {
    if values.length() == 0 {
      0.0
    } else {
      let sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
      sum / values.length().to_float()
    }
  }
  
  // 计算中位数
  let calculate_median = fn(values: Array[Float]) {
    if values.length() == 0 {
      0.0
    } else {
      let sorted_values = values.sort(fn(a, b) { a <= b })
      let n = sorted_values.length()
      
      if n % 2 == 0 {
        (sorted_values[n/2 - 1] + sorted_values[n/2]) / 2.0
      } else {
        sorted_values[n/2]
      }
    }
  }
  
  // 计算标准差
  let calculate_std_dev = fn(values: Array[Float]) {
    if values.length() <= 1 {
      0.0
    } else {
      let mean = calculate_mean(values)
      let squared_diffs = values.map(fn(val) { (val - mean) * (val - mean) })
      let variance = calculate_mean(squared_diffs)
      variance.sqrt()
    }
  }
  
  // 计算百分位数
  let calculate_percentile = fn(values: Array[Float], percentile: Float) {
    if values.length() == 0 {
      0.0
    } else {
      let sorted_values = values.sort(fn(a, b) { a <= b })
      let n = sorted_values.length()
      let index = (percentile / 100.0 * (n - 1).to_float()).to_int()
      
      if index >= n - 1 {
        sorted_values[n - 1]
      } else {
        let lower = sorted_values[index]
        let upper = sorted_values[index + 1]
        let fraction = (percentile / 100.0 * (n - 1).to_float()) - index.to_float()
        lower + fraction * (upper - lower)
      }
    }
  }
  
  // 检测趋势
  let detect_trend = fn(series: TimeSeries) {
    if series.points.length() < 2 {
      "stable"
    } else {
      // 简单线性回归
      let n = series.points.length().to_float()
      let mut sum_x = 0.0
      let mut sum_y = 0.0
      let mut sum_xy = 0.0
      let mut sum_x2 = 0.0
      
      for i in 0..series.points.length() {
        let x = i.to_float()
        let y = series.points[i].value
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = sum_xy + x * y
        sum_x2 = sum_x2 + x * x
      }
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      
      if slope > 0.1 {
        "increasing"
      } else if slope < -0.1 {
        "decreasing"
      } else {
        "stable"
      }
    }
  }
  
  // 检测季节性
  let detect_seasonality = fn(series: TimeSeries) {
    if series.points.length() < 24 {  // 至少需要24个数据点
      false
    } else {
      // 简化的季节性检测：比较不同周期的相关性
      let values = series.points.map(fn(p) { p.value })
      let half_length = values.length() / 2
      let first_half = values.slice(0, half_length)
      let second_half = values.slice(half_length, values.length())
      
      // 计算两个半段的相关系数
      let mean1 = calculate_mean(first_half)
      let mean2 = calculate_mean(second_half)
      
      let mut numerator = 0.0
      let mut sum_sq1 = 0.0
      let mut sum_sq2 = 0.0
      
      for i in 0..half_length {
        let diff1 = first_half[i] - mean1
        let diff2 = second_half[i] - mean2
        numerator = numerator + diff1 * diff2
        sum_sq1 = sum_sq1 + diff1 * diff1
        sum_sq2 = sum_sq2 + diff2 * diff2
      }
      
      let correlation = if sum_sq1 > 0.0 and sum_sq2 > 0.0 {
        numerator / (sum_sq1.sqrt() * sum_sq2.sqrt())
      } else {
        0.0
      }
      
      correlation.abs() > 0.7  # 高相关性表示存在季节性
    }
  }
  
  // 检测异常值
  let detect_outliers = fn(series: TimeSeries) {
    let values = series.points.map(fn(p) { p.value })
    let mean = calculate_mean(values)
    let std_dev = calculate_std_dev(values)
    
    let mut outliers = []
    
    for i in 0..values.length() {
      let z_score = (values[i] - mean) / std_dev
      if z_score.abs() > 2.5 {  # 2.5个标准差之外视为异常
        outliers = outliers.push(i)
      }
    }
    
    outliers
  }
  
  // 分析时间序列
  let analyze_time_series = fn(series: TimeSeries) {
    let values = series.points.map(fn(p) { p.value })
    
    let statistics = {
      mean: calculate_mean(values),
      median: calculate_median(values),
      std_dev: calculate_std_dev(values),
      min: if values.length() > 0 { values.reduce(fn(acc, val) { if val < acc { val } else { acc } }, Float::max_value) } else { 0.0 },
      max: if values.length() > 0 { values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, Float::min_value) } else { 0.0 },
      percentiles: {
        "p50": calculate_percentile(values, 50.0),
        "p90": calculate_percentile(values, 90.0),
        "p95": calculate_percentile(values, 95.0),
        "p99": calculate_percentile(values, 99.0)
      }
    }
    
    {
      trend: detect_trend(series),
      seasonality: detect_seasonality(series),
      outliers: detect_outliers(series),
      statistics
    }
  }
  
  // 创建测试时间序列数据
  let mut points = []
  
  // 添加趋势数据
  for i in 0..50 {
    let value = 100.0 + i.to_float() * 2.0  # 上升趋势
    let point = create_time_series_point(1640995200 + i * 60, value)
    points = points.push(point)
  }
  
  // 添加一些季节性变化
  for i in 0..50 {
    let seasonal_component = 10.0 * (i % 12).to_float().sin()
    points[i] = { points[i] | value: points[i].value + seasonal_component }
  }
  
  // 添加异常值
  points[10] = { points[10] | value: 500.0 }  # 异常高值
  points[30] = { points[30] | value: 50.0 }   # 异常低值
  
  let time_series = create_time_series("cpu_usage", points, "1m", "percent")
  
  // 分析时间序列
  let analysis = analyze_time_series(time_series)
  
  // 验证分析结果
  assert_eq(analysis.trend, "increasing")
  assert_true(analysis.seasonality)
  assert_eq(analysis.outliers.length(), 2)
  assert_true(analysis.outliers.contains(10))
  assert_true(analysis.outliers.contains(30))
  
  // 验证统计信息
  assert_true(analysis.statistics.mean > 100.0)
  assert_true(analysis.statistics.median > 100.0)
  assert_true(analysis.statistics.std_dev > 0.0)
  assert_eq(analysis.statistics.min, 50.0)
  assert_eq(analysis.statistics.max, 500.0)
  
  // 验证百分位数
  assert_true(analysis.statistics.percentiles.get("p50") != None)
  assert_true(analysis.statistics.percentiles.get("p90") != None)
  assert_true(analysis.statistics.percentiles.get("p95") != None)
  assert_true(analysis.statistics.percentiles.get("p99") != None)
}

// 测试2: 聚类分析算法
test "聚类分析算法测试" {
  // 定义数据点
  type DataPoint = {
    id: String,
    features: Array[Float],
    cluster_id: Option[Int]
  }
  
  // 定义聚类中心
  type ClusterCenter = {
    id: Int,
    center: Array[Float],
    points: Array[String]  # 点ID列表
  }
  
  // 定义聚类结果
  type ClusteringResult = {
    clusters: Array[ClusterCenter],
    iterations: Int,
    converged: Bool,
    within_cluster_sum_of_squares: Float
  }
  
  // 计算欧几里得距离
  let euclidean_distance = fn(point1: Array[Float], point2: Array[Float]) {
    if point1.length() != point2.length() {
      Float::max_value
    } else {
      let mut sum_sq = 0.0
      for i in 0..point1.length() {
        let diff = point1[i] - point2[i]
        sum_sq = sum_sq + diff * diff
      }
      sum_sq.sqrt()
    }
  }
  
  // 计算曼哈顿距离
  let manhattan_distance = fn(point1: Array[Float], point2: Array[Float]) {
    if point1.length() != point2.length() {
      Float::max_value
    } else {
      let mut sum_abs = 0.0
      for i in 0..point1.length() {
        sum_abs = sum_abs + (point1[i] - point2[i]).abs()
      }
      sum_abs
    }
  }
  
  // 计算两个点的平均
  let calculate_centroid = fn(points: Array[Array[Float]]) {
    if points.length() == 0 {
      []
    } else {
      let dimension = points[0].length()
      let mut centroid = []
      
      for d in 0..dimension {
        let mut sum = 0.0
        for p in points {
          sum = sum + p[d]
        }
        centroid = centroid.push(sum / points.length().to_float())
      }
      
      centroid
    }
  }
  
  // K-means聚类算法
  let kmeans_clustering = fn(data_points: Array[DataPoint], k: Int, max_iterations: Int) {
    if data_points.length() < k or k <= 0 {
      {
        clusters: [],
        iterations: 0,
        converged: false,
        within_cluster_sum_of_squares: 0.0
      }
    } else {
      // 随机初始化聚类中心
      let mut centers = []
      let used_indices = []
      
      for i in 0..k {
        let mut center_index = Random::int(0, data_points.length() - 1)
        
        // 确保不重复选择相同的点作为中心
        while used_indices.contains(center_index) {
          center_index = Random::int(0, data_points.length() - 1)
        }
        
        used_indices = used_indices.push(center_index)
        centers = centers.push({
          id: i,
          center: data_points[center_index].features,
          points: []
        })
      }
      
      let mut iterations = 0
      let mut converged = false
      let mut current_points = data_points
      
      while iterations < max_iterations and not(converged) {
        // 分配点到最近的聚类中心
        let mut new_points = []
        let mut new_centers = []
        
        for i in 0..k {
          new_centers = new_centers.push({
            id: centers[i].id,
            center: centers[i].center,
            points: []
          })
        }
        
        for point in current_points {
          let mut min_distance = Float::max_value
          let mut closest_cluster = 0
          
          for i in 0..k {
            let distance = euclidean_distance(point.features, centers[i].center)
            if distance < min_distance {
              min_distance = distance
              closest_cluster = i
            }
          }
          
          new_points = new_points.push({ point | cluster_id: Some(closest_cluster) })
          new_centers[closest_cluster] = {
            id: new_centers[closest_cluster].id,
            center: new_centers[closest_cluster].center,
            points: new_centers[closest_cluster].points.push(point.id)
          }
        }
        
        // 更新聚类中心
        let mut updated_centers = []
        let mut centers_changed = false
        
        for i in 0..k {
          let cluster_points = new_points.filter_fn(p) {
            match p.cluster_id {
              Some(id) => id == i
              None => false
            }
          }
          
          if cluster_points.length() > 0 {
            let point_features = cluster_points.map(fn(p) { p.features })
            let new_center = calculate_centroid(point_features)
            
            let distance = euclidean_distance(centers[i].center, new_center)
            if distance > 0.001 {  # 中心变化阈值
              centers_changed = true
            }
            
            updated_centers = updated_centers.push({
              id: centers[i].id,
              center: new_center,
              points: new_centers[i].points
            })
          } else {
            updated_centers = updated_centers.push(centers[i])
          }
        }
        
        centers = updated_centers
        current_points = new_points
        iterations = iterations + 1
        converged = not(centers_changed)
      }
      
      // 计算聚类内平方和
      let mut wcss = 0.0
      
      for point in current_points {
        match point.cluster_id {
          Some(cluster_id) => {
            let distance = euclidean_distance(point.features, centers[cluster_id].center)
            wcss = wcss + distance * distance
          }
          None => {}
        }
      }
      
      {
        clusters: centers,
        iterations,
        converged,
        within_cluster_sum_of_squares: wcss
      }
    }
  }
  
  // 创建测试数据
  let mut data_points = []
  
  // 第一个聚类
  for i in 0..20 {
    let point = {
      id: "point-" + i.to_string(),
      features: [Random::float() * 2.0 + 1.0, Random::float() * 2.0 + 1.0],  # 围绕(2,2)
      cluster_id: None
    }
    data_points = data_points.push(point)
  }
  
  // 第二个聚类
  for i in 20..40 {
    let point = {
      id: "point-" + i.to_string(),
      features: [Random::float() * 2.0 + 8.0, Random::float() * 2.0 + 8.0],  # 围绕(9,9)
      cluster_id: None
    }
    data_points = data_points.push(point)
  }
  
  // 第三个聚类
  for i in 40..60 {
    let point = {
      id: "point-" + i.to_string(),
      features: [Random::float() * 2.0 + 5.0, Random::float() * 2.0 - 2.0],  # 围绕(6,-1)
      cluster_id: None
    }
    data_points = data_points.push(point)
  }
  
  // 执行K-means聚类
  let result = kmeans_clustering(data_points, 3, 100)
  
  // 验证聚类结果
  assert_eq(result.clusters.length(), 3)
  assert_true(result.iterations > 0)
  assert_true(result.converged)
  assert_true(result.within_cluster_sum_of_squares > 0.0)
  
  // 验证每个聚类都有点
  for cluster in result.clusters {
    assert_true(cluster.points.length() > 0)
  }
  
  // 验证所有点都被分配到聚类
  let mut total_assigned_points = 0
  for cluster in result.clusters {
    total_assigned_points = total_assigned_points + cluster.points.length()
  }
  assert_eq(total_assigned_points, 60)
  
  // 测试距离计算
  let point1 = [1.0, 2.0, 3.0]
  let point2 = [4.0, 6.0, 8.0]
  
  let euclidean_dist = euclidean_distance(point1, point2)
  let manhattan_dist = manhattan_distance(point1, point2)
  
  // 验证距离计算
  assert_true(euclidean_dist > 0.0)
  assert_true(manhattan_dist > 0.0)
  
  // 验证欧几里得距离计算
  let expected_euclidean = ((1.0-4.0)*(1.0-4.0) + (2.0-6.0)*(2.0-6.0) + (3.0-8.0)*(3.0-8.0)).sqrt()
  assert_true((euclidean_dist - expected_euclidean).abs() < 0.001)
  
  // 验证曼哈顿距离计算
  let expected_manhattan = (1.0-4.0).abs() + (2.0-6.0).abs() + (3.0-8.0).abs()
  assert_eq(manhattan_dist, expected_manhattan)
}

// 测试3: 关联规则挖掘
test "关联规则挖掘测试" {
  // 定义事务项
  type TransactionItem = String
  
  // 定义事务
  type Transaction = {
    id: String,
    items: Set[TransactionItem]
  }
  
  // 定义项集
  type Itemset = Set[TransactionItem]
  
  // 定义关联规则
  type AssociationRule = {
    antecedent: Itemset,
    consequent: Itemset,
    support: Float,
    confidence: Float,
    lift: Float
  }
  
  // 定义频繁项集
  type FrequentItemset = {
    itemset: Itemset,
    support: Float
  }
  
  // 计算支持度
  let calculate_support = fn(transactions: Array[Transaction], itemset: Itemset) {
    let count = transactions.filter_fn(t) {
      itemset.subset_of(t.items)
    }.length()
    
    count.to_float() / transactions.length().to_float()
  }
  
  // 计算置信度
  let calculate_confidence = fn(transactions: Array[Transaction], antecedent: Itemset, consequent: Itemset) {
    let antecedent_support = calculate_support(transactions, antecedent)
    let combined_support = calculate_support(transactions, antecedent.union(consequent))
    
    if antecedent_support == 0.0 {
      0.0
    } else {
      combined_support / antecedent_support
    }
  }
  
  // 计算提升度
  let calculate_lift = fn(transactions: Array[Transaction], antecedent: Itemset, consequent: Itemset) {
    let antecedent_support = calculate_support(transactions, antecedent)
    let consequent_support = calculate_support(transactions, consequent)
    let combined_support = calculate_support(transactions, antecedent.union(consequent))
    
    if antecedent_support * consequent_support == 0.0 {
      0.0
    } else {
      combined_support / (antecedent_support * consequent_support)
    }
  }
  
  // 生成候选1项集
  let generate_candidate_1_itemsets = fn(transactions: Array[Transaction]) {
    let mut all_items = Set::new()
    
    for transaction in transactions {
      all_items = all_items.union(transaction.items)
    }
    
    let mut itemsets = []
    for item in all_items {
      itemsets = itemsets.push(Set::singleton(item))
    }
    
    itemsets
  }
  
  // 生成候选k项集
  let generate_candidate_itemsets = fn(frequent_itemsets: Array[FrequentItemset], k: Int) {
    let mut candidates = []
    
    for i in 0..frequent_itemsets.length() {
      for j in i+1..frequent_itemsets.length() {
        let itemset1 = frequent_itemsets[i].itemset
        let itemset2 = frequent_itemsets[j].itemset
        
        // 如果两个项集的前k-2项相同，可以合并
        if itemset1.size() >= k-1 and itemset2.size() >= k-1 {
          let union = itemset1.union(itemset2)
          
          if union.size() == k {
            candidates = candidates.push(union)
          }
        }
      }
    }
    
    candidates
  }
  
  // 发现频繁项集（Apriori算法）
  let find_frequent_itemsets = fn(transactions: Array[Transaction], min_support: Float) {
    let mut all_frequent_itemsets = []
    
    // 生成1项集
    let candidate_1_itemsets = generate_candidate_1_itemsets(transactions)
    let mut frequent_1_itemsets = []
    
    for itemset in candidate_1_itemsets {
      let support = calculate_support(transactions, itemset)
      if support >= min_support {
        frequent_1_itemsets = frequent_1_itemsets.push({
          itemset,
          support
        })
      }
    }
    
    all_frequent_itemsets = all_frequent_itemsets + frequent_1_itemsets
    
    // 生成k项集（k >= 2）
    let mut k = 2
    let mut current_frequent_itemsets = frequent_1_itemsets
    
    while current_frequent_itemsets.length() > 0 {
      let candidate_itemsets = generate_candidate_itemsets(current_frequent_itemsets, k)
      let mut next_frequent_itemsets = []
      
      for itemset in candidate_itemsets {
        let support = calculate_support(transactions, itemset)
        if support >= min_support {
          next_frequent_itemsets = next_frequent_itemsets.push({
            itemset,
            support
          })
        }
      }
      
      all_frequent_itemsets = all_frequent_itemsets + next_frequent_itemsets
      current_frequent_itemsets = next_frequent_itemsets
      k = k + 1
    }
    
    all_frequent_itemsets
  }
  
  // 生成关联规则
  let generate_association_rules = fn(transactions: Array[Transaction], frequent_itemsets: Array[FrequentItemset], min_confidence: Float) {
    let mut rules = []
    
    for frequent_itemset in frequent_itemsets {
      if frequent_itemset.itemset.size() >= 2 {
        // 生成所有可能的规则
        let items = frequent_itemset.itemset.to_array()
        
        // 生成所有非空真子集作为前件
        for i in 1..(1 << items.length()) - 1 {
          let mut antecedent = Set::new()
          let mut consequent = Set::new()
          
          for j in 0..items.length() {
            if (i >> j) & 1 == 1 {
              antecedent = antecedent.add(items[j])
            } else {
              consequent = consequent.add(items[j])
            }
          }
          
          // 确保前件和后件都非空
          if antecedent.size() > 0 and consequent.size() > 0 {
            let confidence = calculate_confidence(transactions, antecedent, consequent)
            
            if confidence >= min_confidence {
              let lift = calculate_lift(transactions, antecedent, consequent)
              
              rules = rules.push({
                antecedent,
                consequent,
                support: frequent_itemset.support,
                confidence,
                lift
              })
            }
          }
        }
      }
    }
    
    rules
  }
  
  // 创建测试事务数据
  let transactions = [
    {
      id: "T1",
      items: Set::from_array(["A", "B", "C"])
    },
    {
      id: "T2",
      items: Set::from_array(["A", "B"])
    },
    {
      id: "T3",
      items: Set::from_array(["B", "C", "D"])
    },
    {
      id: "T4",
      items: Set::from_array(["A", "C", "D"])
    },
    {
      id: "T5",
      items: Set::from_array(["A", "B", "C", "D"])
    }
  ]
  
  // 发现频繁项集
  let frequent_itemsets = find_frequent_itemsets(transactions, 0.4)
  
  // 验证频繁项集
  assert_true(frequent_itemsets.length() > 0)
  
  // 验证支持度计算
  let itemset_ab = Set::from_array(["A", "B"])
  let support_ab = calculate_support(transactions, itemset_ab)
  assert_eq(support_ab, 0.6)  # T1, T2, T5包含A和B
  
  // 生成关联规则
  let rules = generate_association_rules(transactions, frequent_itemsets, 0.5)
  
  // 验证关联规则
  assert_true(rules.length() > 0)
  
  // 验证规则指标
  for rule in rules {
    assert_true(rule.support >= 0.4)  # 支持度不低于最小支持度
    assert_true(rule.confidence >= 0.5)  # 置信度不低于最小置信度
    assert_true(rule.lift > 0.0)
  }
  
  // 验证置信度计算
  let antecedent_a = Set::singleton("A")
  let consequent_b = Set::singleton("B")
  let confidence_a_to_b = calculate_confidence(transactions, antecedent_a, consequent_b)
  assert_eq(confidence_a_to_b, 0.75)  # 包含A的事务中，75%也包含B
  
  // 验证提升度计算
  let lift_a_to_b = calculate_lift(transactions, antecedent_a, consequent_b)
  assert_true(lift_a_to_b > 0.0)
  
  // 验证规则生成
  let rule_ab_to_c = rules.find_fn(r) {
    r.antecedent == Set::from_array(["A", "B"]) and r.consequent == Set::singleton("C")
  }
  
  assert_true(rule_ab_to_c != None)
}

// 测试4: 主成分分析(PCA)
test "主成分分析测试" {
  // 定义数据矩阵（每行是一个观测值，每列是一个特征）
  type DataMatrix = {
    rows: Int,
    columns: Int,
    data: Array[Array[Float]]
  }
  
  // 定义PCA结果
  type PCAResult = {
    principal_components: Array[Array[Float]],  # 主成分向量
    explained_variance: Array[Float],           # 解释方差
    explained_variance_ratio: Array[Float],     # 解释方差比例
    cumulative_variance_ratio: Array[Float]     # 累积解释方差比例
  }
  
  // 创建数据矩阵
  let create_data_matrix = fn(data: Array[Array[Float]]) {
    if data.length() == 0 {
      {
        rows: 0,
        columns: 0,
        data: []
      }
    } else {
      {
        rows: data.length(),
        columns: data[0].length(),
        data
      }
    }
  }
  
  // 矩阵转置
  let transpose_matrix = fn(matrix: DataMatrix) {
    let mut transposed_data = []
    
    for j in 0..matrix.columns {
      let mut row = []
      for i in 0..matrix.rows {
        row = row.push(matrix.data[i][j])
      }
      transposed_data = transposed_data.push(row)
    }
    
    {
      rows: matrix.columns,
      columns: matrix.rows,
      data: transposed_data
    }
  }
  
  // 计算均值向量
  let calculate_mean_vector = fn(matrix: DataMatrix) {
    let mut means = []
    
    for j in 0..matrix.columns {
      let mut sum = 0.0
      for i in 0..matrix.rows {
        sum = sum + matrix.data[i][j]
      }
      means = means.push(sum / matrix.rows.to_float())
    }
    
    means
  }
  
  // 数据中心化
  let center_data = fn(matrix: DataMatrix) {
    let means = calculate_mean_vector(matrix)
    let mut centered_data = []
    
    for i in 0..matrix.rows {
      let mut row = []
      for j in 0..matrix.columns {
        row = row.push(matrix.data[i][j] - means[j])
      }
      centered_data = centered_data.push(row)
    }
    
    {
      rows: matrix.rows,
      columns: matrix.columns,
      data: centered_data
    }
  }
  
  // 计算协方差矩阵
  let calculate_covariance_matrix = fn(matrix: DataMatrix) {
    let centered_matrix = center_data(matrix)
    let transposed = transpose_matrix(centered_matrix)
    
    // 协方差矩阵 = (1/(n-1)) * X^T * X
    let n = matrix.rows.to_float()
    let mut covariance_data = []
    
    for i in 0..transposed.rows {
      let mut row = []
      for j in 0..transposed.columns {
        let mut sum = 0.0
        for k in 0..centered_matrix.rows {
          sum = sum + transposed.data[i][k] * centered_matrix.data[k][j]
        }
        row = row.push(sum / (n - 1.0))
      }
      covariance_data = covariance_data.push(row)
    }
    
    {
      rows: transposed.rows,
      columns: transposed.columns,
      data: covariance_data
    }
  }
  
  // 计算特征值和特征向量（简化实现）
  let calculate_eigenvalues_eigenvectors = fn(matrix: DataMatrix) {
    // 简化实现：返回对角线元素作为特征值，单位矩阵作为特征向量
    // 实际实现需要使用更复杂的算法如幂迭代法或QR分解
    
    let mut eigenvalues = []
    let mut eigenvectors = []
    
    for i in 0..matrix.rows {
      eigenvalues = eigenvalues.push(matrix.data[i][i])
      
      let mut eigenvector = []
      for j in 0..matrix.rows {
        eigenvector = eigenvector.push(if i == j { 1.0 } else { 0.0 })
      }
      eigenvectors = eigenvectors.push(eigenvector)
    }
    
    (eigenvalues, eigenvectors)
  }
  
  // 执行PCA
  let perform_pca = fn(matrix: DataMatrix, n_components: Int) {
    if matrix.rows == 0 or matrix.columns == 0 or n_components <= 0 or n_components > matrix.columns {
      {
        principal_components: [],
        explained_variance: [],
        explained_variance_ratio: [],
        cumulative_variance_ratio: []
      }
    } else {
      // 计算协方差矩阵
      let covariance_matrix = calculate_covariance_matrix(matrix)
      
      // 计算特征值和特征向量
      let (eigenvalues, eigenvectors) = calculate_eigenvalues_eigenvectors(covariance_matrix)
      
      // 按特征值降序排序
      let mut indexed_eigenvalues = []
      for i in 0..eigenvalues.length() {
        indexed_eigenvalues = indexed_eigenvalues.push((i, eigenvalues[i]))
      }
      
      let sorted_eigenvalues = indexed_eigenvalues.sort(fn(a, b) { b.1 > a.1 })
      
      // 选择前n_components个主成分
      let mut principal_components = []
      let mut explained_variance = []
      let mut total_variance = 0.0
      
      for i in 0..eigenvalues.length() {
        total_variance = total_variance + eigenvalues[i]
      }
      
      for i in 0..n_components.min(sorted_eigenvalues.length()) {
        let eigenvalue_index = sorted_eigenvalues[i].0
        let eigenvalue = sorted_eigenvalues[i].1
        
        principal_components = principal_components.push(eigenvectors[eigenvalue_index])
        explained_variance = explained_variance.push(eigenvalue)
      }
      
      // 计算解释方差比例和累积比例
      let mut explained_variance_ratio = []
      let mut cumulative_variance_ratio = []
      let mut cumulative = 0.0
      
      for variance in explained_variance {
        let ratio = variance / total_variance
        cumulative = cumulative + ratio
        
        explained_variance_ratio = explained_variance_ratio.push(ratio)
        cumulative_variance_ratio = cumulative_variance_ratio.push(cumulative)
      }
      
      {
        principal_components,
        explained_variance,
        explained_variance_ratio,
        cumulative_variance_ratio
      }
    }
  }
  
  // 创建测试数据
  let data = [
    [2.5, 2.4],
    [0.5, 0.7],
    [2.2, 2.9],
    [1.9, 2.2],
    [3.1, 3.0],
    [2.3, 2.7],
    [2.0, 1.6],
    [1.0, 1.1],
    [1.5, 1.6],
    [1.1, 0.9]
  ]
  
  let matrix = create_data_matrix(data)
  
  // 执行PCA
  let pca_result = perform_pca(matrix, 2)
  
  // 验证PCA结果
  assert_eq(pca_result.principal_components.length(), 2)
  assert_eq(pca_result.explained_variance.length(), 2)
  assert_eq(pca_result.explained_variance_ratio.length(), 2)
  assert_eq(pca_result.cumulative_variance_ratio.length(), 2)
  
  // 验证每个主成分的维度
  for component in pca_result.principal_components {
    assert_eq(component.length(), 2)  # 原始数据有2个特征
  }
  
  // 验证解释方差比例
  for ratio in pca_result.explained_variance_ratio {
    assert_true(ratio >= 0.0 and ratio <= 1.0)
  }
  
  // 验证累积解释方差比例
  for i in 0..pca_result.cumulative_variance_ratio.length() {
    assert_true(pca_result.cumulative_variance_ratio[i] >= 0.0)
    assert_true(pca_result.cumulative_variance_ratio[i] <= 1.0)
    
    if i > 0 {
      assert_true(pca_result.cumulative_variance_ratio[i] >= pca_result.cumulative_variance_ratio[i-1])
    }
  }
  
  // 验证最后一个累积比例应该接近1.0
  let last_cumulative = pca_result.cumulative_variance_ratio[pca_result.cumulative_variance_ratio.length() - 1]
  assert_true((last_cumulative - 1.0).abs() < 0.001)
  
  // 测试使用1个主成分
  let pca_result_1 = perform_pca(matrix, 1)
  
  assert_eq(pca_result_1.principal_components.length(), 1)
  assert_eq(pca_result_1.explained_variance.length(), 1)
  assert_eq(pca_result_1.explained_variance_ratio.length(), 1)
  assert_eq(pca_result_1.cumulative_variance_ratio.length(), 1)
  
  // 验证单个主成分的维度
  assert_eq(pca_result_1.principal_components[0].length(), 2)
}

// 测试5: 异常检测算法
test "异常检测算法测试" {
  // 定义数据点
  type DataPoint = {
    id: String,
    features: Array[Float],
    is_outlier: Option[Bool]
  }
  
  // 定义异常检测结果
  type AnomalyDetectionResult = {
    outliers: Array[String],  # 异常点ID
    scores: Array[Float],     # 异常分数
    threshold: Float
  }
  
  // 计算欧几里得距离
  let euclidean_distance = fn(point1: Array[Float], point2: Array[Float]) {
    if point1.length() != point2.length() {
      Float::max_value
    } else {
      let mut sum_sq = 0.0
      for i in 0..point1.length() {
        let diff = point1[i] - point2[i]
        sum_sq = sum_sq + diff * diff
      }
      sum_sq.sqrt()
    }
  }
  
  // 计算平均值
  let calculate_mean = fn(values: Array[Float]) {
    if values.length() == 0 {
      0.0
    } else {
      let sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
      sum / values.length().to_float()
    }
  }
  
  // 计算标准差
  let calculate_std_dev = fn(values: Array[Float]) {
    if values.length() <= 1 {
      0.0
    } else {
      let mean = calculate_mean(values)
      let squared_diffs = values.map(fn(val) { (val - mean) * (val - mean) })
      let variance = calculate_mean(squared_diffs)
      variance.sqrt()
    }
  }
  
  // Z-score异常检测
  let zscore_outlier_detection = fn(data_points: Array[DataPoint], threshold: Float) {
    let mut outliers = []
    let mut scores = []
    
    // 对每个特征维度计算Z-score
    for point in data_points {
      let mut max_zscore = 0.0
      
      for d in 0..point.features.length() {
        let values = data_points.map_fn(p) { p.features[d] }
        let mean = calculate_mean(values)
        let std_dev = calculate_std_dev(values)
        
        if std_dev > 0.0 {
          let zscore = (point.features[d] - mean) / std_dev
          if zscore.abs() > max_zscore {
            max_zscore = zscore.abs()
          }
        }
      }
      
      scores = scores.push(max_zscore)
      
      if max_zscore > threshold {
        outliers = outliers.push(point.id)
      }
    }
    
    {
      outliers,
      scores,
      threshold
    }
  }
  
  // KNN距离异常检测
  let knn_outlier_detection = fn(data_points: Array[DataPoint], k: Int, threshold: Float) {
    let mut outliers = []
    let mut scores = []
    
    for i in 0..data_points.length() {
      let point = data_points[i]
      
      // 计算到所有其他点的距离
      let mut distances = []
      for j in 0..data_points.length() {
        if i != j {
          let distance = euclidean_distance(point.features, data_points[j].features)
          distances = distances.push(distance)
        }
      }
      
      // 排序并取前k个最近邻
      let sorted_distances = distances.sort(fn(a, b) { a <= b })
      let k_nearest = sorted_distances.slice(0, k.min(sorted_distances.length()))
      
      // 计算平均距离作为异常分数
      let avg_distance = if k_nearest.length() > 0 {
        calculate_mean(k_nearest)
      } else {
        0.0
      }
      
      scores = scores.push(avg_distance)
      
      if avg_distance > threshold {
        outliers = outliers.push(point.id)
      }
    }
    
    {
      outliers,
      scores,
      threshold
    }
  }
  
  // 孤立森林异常检测（简化实现）
  let isolation_forest_outlier_detection = fn(data_points: Array[DataPoint], num_trees: Int, threshold: Float) {
    let mut scores = []
    
    // 简化实现：基于随机投影和深度
    for point in data_points {
      let mut total_depth = 0.0
      
      for t in 0..num_trees {
        // 随机选择一个特征和分割点
        let feature_index = Random::int(0, point.features.length() - 1)
        let values = data_points.map_fn(p) { p.features[feature_index] }
        let min_val = values.reduce(fn(acc, val) { if val < acc { val } else { acc } }, Float::max_value)
        let max_val = values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, Float::min_value)
        
        let split_point = Random::float(min_val, max_val)
        
        // 计算深度
        let depth = if point.features[feature_index] <= split_point {
          Random::float(1.0, 10.0)  # 简化的深度计算
        } else {
          Random::float(1.0, 10.0)
        }
        
        total_depth = total_depth + depth
      }
      
      let avg_depth = total_depth / num_trees.to_float()
      let score = 2.0 ^ (-avg_depth / 5.0)  # 简化的异常分数
      
      scores = scores.push(score)
    }
    
    // 根据阈值识别异常
    let mut outliers = []
    for i in 0..data_points.length() {
      if scores[i] > threshold {
        outliers = outliers.push(data_points[i].id)
      }
    }
    
    {
      outliers,
      scores,
      threshold
    }
  }
  
  // 创建测试数据
  let mut normal_points = []
  
  // 生成正常数据点（围绕原点的正态分布）
  for i in 0..50 {
    let point = {
      id: "normal-" + i.to_string(),
      features: [Random::float() * 4.0 - 2.0, Random::float() * 4.0 - 2.0],  # [-2, 2]范围
      is_outlier: None
    }
    normal_points = normal_points.push(point)
  }
  
  // 生成异常数据点
  let mut outlier_points = []
  
  // 添加一些明显的异常点
  outlier_points = outlier_points.push({
    id: "outlier-1",
    features: [10.0, 10.0],  # 远离正常范围
    is_outlier: None
  })
  
  outlier_points = outlier_points.push({
    id: "outlier-2",
    features: [-8.0, -8.0],  # 远离正常范围
    is_outlier: None
  })
  
  outlier_points = outlier_points.push({
    id: "outlier-3",
    features: [0.0, 15.0],  # 一个维度异常
    is_outlier: None
  })
  
  let all_points = normal_points + outlier_points
  
  // 测试Z-score异常检测
  let zscore_result = zscore_outlier_detection(all_points, 2.5)
  
  // 验证Z-score结果
  assert_true(zscore_result.outliers.length() >= 3)  # 至少检测到3个异常点
  assert_eq(zscore_result.scores.length(), all_points.length())
  assert_eq(zscore_result.threshold, 2.5)
  
  // 验证异常点被检测到
  assert_true(zscore_result.outliers.contains("outlier-1"))
  assert_true(zscore_result.outliers.contains("outlier-2"))
  assert_true(zscore_result.outliers.contains("outlier-3"))
  
  // 测试KNN异常检测
  let knn_result = knn_outlier_detection(all_points, 5, 5.0)
  
  // 验证KNN结果
  assert_true(knn_result.outliers.length() >= 2)  # 至少检测到2个异常点
  assert_eq(knn_result.scores.length(), all_points.length())
  assert_eq(knn_result.threshold, 5.0)
  
  // 验证异常点被检测到
  assert_true(knn_result.outliers.contains("outlier-1"))
  assert_true(knn_result.outliers.contains("outlier-2"))
  
  // 测试孤立森林异常检测
  let isolation_result = isolation_forest_outlier_detection(all_points, 10, 0.5)
  
  // 验证孤立森林结果
  assert_true(isolation_result.outliers.length() >= 2)  # 至少检测到2个异常点
  assert_eq(isolation_result.scores.length(), all_points.length())
  assert_eq(isolation_result.threshold, 0.5)
  
  // 验证异常分数范围
  for score in isolation_result.scores {
    assert_true(score >= 0.0 and score <= 1.0)
  }
  
  // 验证异常点被检测到
  assert_true(isolation_result.outliers.contains("outlier-1") or isolation_result.outliers.contains("outlier-2"))
  
  // 比较不同方法的检测结果
  let zscore_outlier_count = zscore_result.outliers.length()
  let knn_outlier_count = knn_result.outliers.length()
  let isolation_outlier_count = isolation_result.outliers.length()
  
  // 不同方法可能检测到不同数量的异常点，但都应该检测到一些
  assert_true(zscore_outlier_count > 0)
  assert_true(knn_outlier_count > 0)
  assert_true(isolation_outlier_count > 0)
}