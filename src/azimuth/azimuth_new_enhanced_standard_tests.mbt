// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
  assert_eq(1, add(0, 1))  // 单位元测试
  assert_eq(-1, add(0, -1))
}

test "multiply_function_large_numbers" {
  // 测试乘法函数的大数运算
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(4611686014132420609, multiply(2147483647, 2147483647))  // 最大值平方
  assert_eq(-1000000, multiply(1000, -1000))
  assert_eq(0, multiply(2147483647, 0))  // 零乘法
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 整除
  assert_eq(1, divide_with_ceil(1, 100)) // 小数
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // 3.333... -> 4
  assert_eq(0, divide_with_ceil(5, 0))     // 除零保护
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, 🚀 MoonBit! 🌟!", greet("🚀 MoonBit! 🌟"))
  assert_eq_string("Hello, line1\nline2!", greet("line1\nline2"))
  assert_eq_string("Hello, tab\there!", greet("tab\there"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 计算复利（简化版，每年计算一次）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "inventory_warehouse_calculation" {
  // 仓库库存计算
  let warehouse_capacity = 5000
  let item_volume = 12
  let safety_stock = 200
  
  // 计算可存储的物品数量
  let max_items = divide_with_ceil(warehouse_capacity, item_volume)
  let available_space = add(max_items, -divide_with_ceil(safety_stock, item_volume))
  
  assert_eq(417, max_items)  // ceil(5000/12) = 417
  assert_eq(401, available_space)  // 417 - ceil(200/12) = 417 - 17 = 400
}

test "mathematical_properties_verification" {
  // 验证基本数学性质
  let a = 15
  let b = 25
  let c = 35
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
}

test "real_world_shipping_calculation" {
  // 真实世界运输计算
  let package_weight = 47  // kg
  let base_rate = 10  // per kg
  let weight_threshold = 50  // kg
  let discount_rate = 10  // percent
  
  // 计算基础运费
  let base_cost = multiply(package_weight, base_rate)
  
  // 应用折扣（如果超过阈值）
  let discount = if base_cost > multiply(weight_threshold, base_rate) {
    divide_with_ceil(multiply(base_cost, discount_rate), 100)
  } else {
    0
  }
  
  let final_cost = add(base_cost, -discount)
  
  assert_eq(470, base_cost)
  assert_eq(47, discount)
  assert_eq(423, final_cost)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  let input_size = 1000
  let linear_factor = 2
  let quadratic_factor = 3
  let constant_factor = 10
  
  // 模拟 O(n²) 算法的执行时间
  let linear_time = multiply(input_size, linear_factor)
  let quadratic_time = divide_with_ceil(multiply(input_size, input_size), quadratic_factor)
  let total_time = add(add(linear_time, quadratic_time), constant_factor)
  
  assert_eq(2000, linear_time)
  assert_eq(333334, quadratic_time)  // ceil(1000*1000/3) = 333334
  assert_eq(335344, total_time)
}

test "data_structure_operations" {
  // 数据结构操作模拟
  let array_size = 100
  let page_size = 10
  
  // 模拟分页计算
  let total_pages = divide_with_ceil(array_size, page_size)
  
  // 模拟二分查找的中间位置
  let middle_index = divide_with_ceil(array_size, 2)
  
  // 模拟循环移位
  let shift_distance = 25
  let effective_shift = shift_distance % array_size
  let new_position = add(middle_index, effective_shift)
  
  assert_eq(10, total_pages)  // ceil(100/10) = 10
  assert_eq(50, middle_index)  // ceil(100/2) = 50
  assert_eq(75, new_position)  // 50 + 25 = 75
}