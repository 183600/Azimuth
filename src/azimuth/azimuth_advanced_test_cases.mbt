// Azimuth 高级测试用例
// 包含 2 个高级测试用例，总共达到 10 个测试用例的要求

test "algorithm_complexity_analysis" {
  // 算法复杂度分析测试
  let n = 100
  
  // O(1) 常数时间操作
  let constant_result = @azimuth.add(5, 10)
  
  // O(n) 线性时间操作模拟 - 计算 1 到 n 的和
  let sum_1_to_10 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  let sum_11_to_20 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(11, 12), 13), 14), 15), 16), 17), 18), 19), 20)
  let sum_1_to_20 = @azimuth.add(sum_1_to_10, sum_11_to_20)
  
  // O(n²) 二次时间操作模拟
  let quadratic_result = @azimuth.multiply(n, n)
  
  // O(log n) 对数时间操作模拟 - 二分查找步骤
  let low = 0
  let high = 100
  let target = 75
  let mid1 = @azimuth.divide_with_ceil(@azimuth.add(low, high), 2)
  let mid2 = @azimuth.divide_with_ceil(@azimuth.add(mid1, high), 2)
  
  @azimuth.assert_eq(15, constant_result)
  @azimuth.assert_eq(55, sum_1_to_10)
  @azimuth.assert_eq(155, sum_11_to_20)
  @azimuth.assert_eq(210, sum_1_to_20)
  @azimuth.assert_eq(10000, quadratic_result)
  @azimuth.assert_eq(50, mid1)
  @azimuth.assert_eq(75, mid2)
}

test "data_structure_simulation" {
  // 数据结构模拟测试
  // 模拟队列操作
  let queue_size = 0
  let queue_front = 0
  let queue_rear = 0
  
  // enqueue(10)
  let enqueue1 = 10
  let queue_size1 = @azimuth.add(queue_size, 1)
  let queue_rear1 = @azimuth.add(queue_rear, 1)
  
  // enqueue(20)
  let enqueue2 = 20
  let queue_size2 = @azimuth.add(queue_size1, 1)
  let queue_rear2 = @azimuth.add(queue_rear1, 1)
  
  // enqueue(30)
  let enqueue3 = 30
  let queue_size3 = @azimuth.add(queue_size2, 1)
  let queue_rear3 = @azimuth.add(queue_rear2, 1)
  
  // dequeue() -> 应该返回 10
  let dequeue1 = 10
  let queue_size_after_dequeue1 = @azimuth.subtract(queue_size3, 1)
  let queue_front_after_dequeue1 = @azimuth.add(queue_front, 1)
  
  // dequeue() -> 应该返回 20
  let dequeue2 = 20
  let queue_size_after_dequeue2 = @azimuth.subtract(queue_size_after_dequeue1, 1)
  let queue_front_after_dequeue2 = @azimuth.add(queue_front_after_dequeue1, 1)
  
  // 验证队列操作
  @azimuth.assert_eq(1, queue_size1)
  @azimuth.assert_eq(1, queue_rear1)
  @azimuth.assert_eq(2, queue_size2)
  @azimuth.assert_eq(2, queue_rear2)
  @azimuth.assert_eq(3, queue_size3)
  @azimuth.assert_eq(3, queue_rear3)
  @azimuth.assert_eq(2, queue_size_after_dequeue1)
  @azimuth.assert_eq(1, queue_front_after_dequeue1)
  @azimuth.assert_eq(1, queue_size_after_dequeue2)
  @azimuth.assert_eq(2, queue_front_after_dequeue2)
}