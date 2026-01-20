// 额外的 MoonBit 测试用例
// 测试 azimuth 包的额外功能和边界情况

test "division_simulation" {
  // 使用乘法和加法模拟除法运算
  let dividend = 27
  let divisor = 4
  
  // 计算商：27 / 4 = 6（整数除法）
  let quotient = divide_with_ceil(dividend, divisor)
  assert_eq(7, quotient)  // 向上取整
  
  // 测试整除情况
  let exact_dividend = 20
  let exact_divisor = 5
  let exact_quotient = divide_with_ceil(exact_dividend, exact_divisor)
  assert_eq(4, exact_quotient)
}

test "negative_number_operations" {
  // 测试负数运算的各种情况
  let neg_a = -15
  let neg_b = -8
  let pos_a = 12
  let pos_b = 7
  
  // 负数相加
  assert_eq(-23, add(neg_a, neg_b))
  
  // 正负数相加
  assert_eq(-3, add(neg_a, pos_a))
  assert_eq(-1, add(neg_b, pos_b))
  
  // 负数相乘
  assert_eq(120, multiply(neg_a, neg_b))
  
  // 正负数相乘
  assert_eq(-180, multiply(neg_a, pos_a))
  assert_eq(-96, multiply(neg_b, pos_b))
}

test "string_validation_functions" {
  // 测试字符串验证相关的功能
  let valid_name = "JohnDoe"
  let invalid_name_empty = ""
  let invalid_name_spaces = "   "
  let special_chars_name = "John@123"
  
  // 测试正常名称
  assert_eq_string("Hello, JohnDoe!", greet(valid_name))
  
  // 测试空名称
  assert_eq_string("Hello, !", greet(invalid_name_empty))
  
  // 测试空格名称
  assert_eq_string("Hello,    !", greet(invalid_name_spaces))
  
  // 测试特殊字符名称
  assert_eq_string("Hello, John@123!", greet(special_chars_name))
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 4 + 7 + 10 + 13 = 35
  let n = 5
  let first_term = 1
  let common_diff = 3
  
  // 使用公式：S_n = n/2 * (2a + (n-1)d)
  let sum = multiply(n, add(multiply(2, first_term), multiply(n - 1, common_diff))) / 2
  assert_eq(35, sum)
  
  // 计算等比数列前n项和：2 + 6 + 18 + 54 = 80
  let geo_first = 2
  let geo_ratio = 3
  let geo_n = 4
  
  // 简化计算：手动相加
  let geo_sum = add(add(add(geo_first, multiply(geo_first, geo_ratio)), 
                      multiply(multiply(geo_first, geo_ratio), geo_ratio)),
                  multiply(multiply(multiply(geo_first, geo_ratio), geo_ratio), geo_ratio))
  assert_eq(80, geo_sum)
}

test "business_calculation_scenario" {
  // 测试商业计算场景
  let unit_price = 199
  let quantity = 8
  let discount_rate = 15  // 15% 折扣
  let tax_rate = 8       // 8% 税率
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算折扣金额
  let discount_amount = multiply(subtotal, discount_rate) / 100
  
  // 计算折后价格
  let discounted_price = add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = multiply(discounted_price, tax_rate) / 100
  
  // 计算最终价格
  let final_price = add(discounted_price, tax_amount)
  
  assert_eq(1465, final_price)
}

test "scientific_calculations" {
  // 测试科学计算
  // 计算物体的动能：KE = 1/2 * m * v^2
  let mass = 10
  let velocity = 15
  
  // 简化计算：m * v^2 / 2
  let velocity_squared = multiply(velocity, velocity)
  let kinetic_energy = multiply(multiply(mass, velocity_squared), 50) / 100  // 乘以0.5
  assert_eq(1125, kinetic_energy)
  
  // 计算简单斜抛运动的最大高度：h = v^2 * sin^2(θ) / (2g)
  // 简化计算，假设 sin(θ) = 1，g = 10
  let initial_velocity = 20
  let height = multiply(multiply(initial_velocity, initial_velocity), 50) / 200  // v^2 / 20
  assert_eq(20, height)
}

test "data_structure_operations" {
  // 测试数据结构相关的操作
  // 模拟栈操作：push, pop
  let stack_size = 0
  let element1 = 10
  let element2 = 20
  let element3 = 30
  
  // Push 操作
  stack_size = add(stack_size, 1)  // push element1
  stack_size = add(stack_size, 1)  // push element2
  stack_size = add(stack_size, 1)  // push element3
  
  assert_eq(3, stack_size)
  
  // Pop 操作
  stack_size = add(stack_size, -1)  // pop element3
  stack_size = add(stack_size, -1)  // pop element2
  
  assert_eq(1, stack_size)
  
  // 模拟队列操作：enqueue, dequeue
  let queue_front = 0
  let queue_rear = 0
  let queue_size = 0
  
  // Enqueue 操作
  queue_rear = add(queue_rear, 1)
  queue_size = add(queue_size, 1)
  
  queue_rear = add(queue_rear, 1)
  queue_size = add(queue_size, 1)
  
  assert_eq(2, queue_size)
  
  // Dequeue 操作
  queue_front = add(queue_front, 1)
  queue_size = add(queue_size, -1)
  
  assert_eq(1, queue_size)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度的模拟
  // 模拟 O(n) 时间复杂度：线性搜索
  let n = 100
  let target = 75
  
  // 模拟线性搜索的比较次数
  let comparisons = target  // 最坏情况需要比较 target 次
  assert_true(comparisons <= n)
  
  // 模拟 O(n^2) 时间复杂度：冒泡排序
  let array_size = 10
  // 比较次数：n*(n-1)/2
  let bubble_comparisons = multiply(array_size, array_size - 1) / 2
  assert_eq(45, bubble_comparisons)
  
  // 模拟 O(log n) 时间复杂度：二分搜索
  let log_n_comparisons = 8  // 对于 n=256，最多需要8次比较
  assert_true(log_n_comparisons < n)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续运算的边界情况
  let result1 = add(add(max_val, -1), add(min_val, 1))
  assert_eq(0, result1)
  
  // 测试极值乘法的边界情况
  let result2 = multiply(add(max_val, -max_val), add(min_val, -min_val))
  assert_eq(0, result2)
  
  // 测试混合边界运算
  let result3 = add(multiply(max_val, 0), multiply(min_val, 0))
  assert_eq(0, result3)
  
  // 测试断言函数的边界情况
  assert_true(add(max_val, min_val) == -1)
  assert_false(add(max_val, min_val) == 0)
  assert_eq(-1, add(max_val, min_val))
}