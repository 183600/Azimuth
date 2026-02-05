// 新增的标准 MoonBit 测试用例
// 为 Azimuth 项目添加高质量的测试覆盖

test "enhanced_addition_commutative_property" {
  // 测试加法交换律的更多情况
  @azimuth.assert_eq(@azimuth.add(123, 456), @azimuth.add(456, 123))
  @azimuth.assert_eq(@azimuth.add(-100, 200), @azimuth.add(200, -100))
  @azimuth.assert_eq(@azimuth.add(0, 999), @azimuth.add(999, 0))
  @azimuth.assert_eq(@azimuth.add(-50, -75), @azimuth.add(-75, -50))
}

test "enhanced_multiplication_distributive_property" {
  // 测试乘法分配律
  let a = 7
  let b = 5
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
  
  // (a + b) * c = a * c + b * c
  let left2 = @azimuth.multiply(@azimuth.add(a, b), c)
  let right2 = @azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c))
  @azimuth.assert_eq(left2, right2)
}

test "enhanced_divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(9, 2))
  
  // 测试大数精度
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(2001, 2))
  @azimuth.assert_eq(667, @azimuth.divide_with_ceil(2001, 3))
}

test "enhanced_greet_multilingual_support" {
  // 测试多语言支持的问候函数
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Français!", @azimuth.greet("Français"))
  @azimuth.assert_eq_string("Hello, Deutsch!", @azimuth.greet("Deutsch"))
  @azimuth.assert_eq_string("Hello, Português!", @azimuth.greet("Português"))
  @azimuth.assert_eq_string("Hello, Русский!", @azimuth.greet("Русский"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, 한국어!", @azimuth.greet("한국어"))
}

test "enhanced_complex_business_workflow" {
  // 测试复杂业务工作流
  // 场景：电商订单处理
  let product_price = 299
  let quantity = 3
  let shipping_cost = 50
  let discount_amount = 100
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 加上运费
  let with_shipping = @azimuth.add(subtotal, shipping_cost)
  
  // 减去折扣
  let total = @azimuth.add(with_shipping, -discount_amount)
  
  @azimuth.assert_eq(897, total)  // 299*3 + 50 - 100 = 897
  
  // 计算每个商品的平均成本
  let avg_cost = @azimuth.divide_with_ceil(total, quantity)
  @azimuth.assert_eq(299, avg_cost)
}

test "enhanced_mathematical_sequence" {
  // 测试数学序列计算
  // 斐波那契数列的前几项（简化版）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.add(fib5, fib6)  // 13
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
  @azimuth.assert_eq(13, fib7)
  
  // 测试斐波那契数的性质
  let sum_first_n = @azimuth.add(fib1, @azimuth.add(fib2, @azimuth.add(fib3, @azimuth.add(fib4, fib5))))
  let fib_next_minus_one = @azimuth.add(fib6, -1)
  @azimuth.assert_eq(sum_first_n, fib_next_minus_one)  // 1+1+2+3+5 = 13-1
}

test "enhanced_resource_optimization" {
  // 测试资源优化算法
  let total_tasks = 100
  let team_capacity = 8
  let overtime_threshold = 10
  
  // 计算需要的团队数量
  let teams_needed = @azimuth.divide_with_ceil(total_tasks, team_capacity)
  @azimuth.assert_eq(13, teams_needed)
  
  // 计算总容量
  let total_capacity = @azimuth.multiply(teams_needed, team_capacity)
  @azimuth.assert_eq(104, total_capacity)
  
  // 计算剩余容量
  let remaining_capacity = @azimuth.add(total_capacity, -total_tasks)
  @azimuth.assert_eq(4, remaining_capacity)
  
  // 如果有足够剩余容量，可以减少加班
  @azimuth.assert_eq(true, remaining_capacity >= overtime_threshold)
}

test "enhanced_financial_compounding" {
  // 测试金融复利计算
  let principal = 1000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算（整数运算）
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, interest_rate), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, interest_rate), 100))
  
  @azimuth.assert_eq(1050, year1)  // 1000 + 50
  @azimuth.assert_eq(1103, year2)  // 1050 + 53 (向上取整)
  @azimuth.assert_eq(1158, year3)  // 1103 + 55 (向上取整)
  
  // 计算总收益
  let total_gain = @azimuth.add(year3, -principal)
  @azimuth.assert_eq(158, total_gain)
}

test "enhanced_data_analysis" {
  // 测试数据分析计算
  let data_points = [15, 25, 35, 45, 55]
  
  // 计算总和（简化版，手动相加）
  let sum = @azimuth.add(15, @azimuth.add(25, @azimuth.add(35, @azimuth.add(45, 55))))
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let count = 5
  let average = @azimuth.divide_with_ceil(sum, count)
  @azimuth.assert_eq(35, average)
  
  // 计算中位数（已排序数组）
  let median = 35
  @azimuth.assert_eq(35, median)
  
  // 验证平均值与中位数的关系
  let avg_median_diff = @azimuth.add(average, -median)
  @azimuth.assert_eq(0, avg_median_diff)
}

test "enhanced_logistics_optimization" {
  // 测试物流优化算法
  let packages = [10, 15, 20, 25, 30]
  let truck_capacity = 50
  
  // 计算需要的卡车数量（简化版，累加后计算）
  let total_weight = @azimuth.add(10, @azimuth.add(15, @azimuth.add(20, @azimuth.add(25, 30))))
  @azimuth.assert_eq(100, total_weight)
  
  let trucks_needed = @azimuth.divide_with_ceil(total_weight, truck_capacity)
  @azimuth.assert_eq(2, trucks_needed)
  
  // 计算总运力
  let total_capacity = @azimuth.multiply(trucks_needed, truck_capacity)
  @azimuth.assert_eq(100, total_capacity)
  
  // 计算运力利用率
  let utilization_percent = @azimuth.divide_with_ceil(@azimuth.multiply(total_weight, 100), total_capacity)
  @azimuth.assert_eq(100, utilization_percent)
  
  // 测试如果增加一个包裹后的情况
  let additional_package = 5
  let new_total = @azimuth.add(total_weight, additional_package)
  let new_trucks_needed = @azimuth.divide_with_ceil(new_total, truck_capacity)
  @azimuth.assert_eq(3, new_trucks_needed)  // 105/50 = 2.1 -> 3
}