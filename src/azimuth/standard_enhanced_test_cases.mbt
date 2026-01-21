// 新增的标准 MoonBit 测试用例
// 覆盖 azimuth 项目的核心功能

test "divide_with_ceil_basic_functionality" {
  // 测试基本的向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(2, divide_with_ceil(5, -3))
}

test "divide_with_ceil_edge_cases" {
  // 测试边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(100, divide_with_ceil(99, 1))
}

test "complex_mathematical_expression" {
  // 测试复杂数学表达式：(a * b + c * d) / e 向上取整
  let a = 7
  let b = 8
  let c = 5
  let d = 6
  let e = 4
  
  let product1 = multiply(a, b)  // 56
  let product2 = multiply(c, d)  // 30
  let sum = add(product1, product2)  // 86
  let result = divide_with_ceil(sum, e)  // 22
  
  assert_eq(22, result)
}

test "financial_calculation_with_ceil" {
  // 测试金融计算场景：计算需要的钞票数量
  let total_amount = 1234
  let bill_denomination = 100
  
  let bills_needed = divide_with_ceil(total_amount, bill_denomination)
  assert_eq(13, bills_needed)  // 需要13张100元钞票
}

test "string_processing_with_numbers" {
  // 测试字符串与数字的组合处理
  let year_str = "2024"
  let month_str = "1"
  let day_str = "21"
  
  let date_string = greet("Date: " + year_str + "-" + month_str + "-" + day_str)
  assert_eq_string("Hello, Date: 2024-1-21!", date_string)
}

test "boundary_value_comprehensive_test" {
  // 综合边界值测试
  let max_val = 2147483647
  
  // 测试极值加法与除法的组合
  let sum1 = add(max_val, -1)  // 2147483646
  let result1 = divide_with_ceil(sum1, 1000000)  // 2148
  assert_eq(2148, result1)
  
  // 测试极值乘法与除法的组合
  let product1 = multiply(10000, 100000)  // 1000000000
  let result2 = divide_with_ceil(product1, 123456789)  // 9
  assert_eq(9, result2)
}

test "geometry_calculation_with_ceil" {
  // 测试几何计算中的向上取整应用
  // 计算铺设地板需要的地砖数量（向上取整）
  let room_length = 350  // cm
  let room_width = 280   // cm
  let tile_size = 30     // cm
  
  let tiles_length = divide_with_ceil(room_length, tile_size)  // 12
  let tiles_width = divide_with_ceil(room_width, tile_size)    // 10
  let total_tiles = multiply(tiles_length, tiles_width)       // 120
  
  assert_eq(120, total_tiles)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟：O(n log n) 近似计算
  let n = 1000
  let log2_approx = 10  // 2^10 = 1024 ≈ 1000
  
  let operations = multiply(n, log2_approx)
  assert_eq(10000, operations)
  
  // 测试向上取整在复杂度计算中的应用
  let chunks = divide_with_ceil(operations, 1000)  // 分成1000个操作的块
  assert_eq(10, chunks)
}

test "error_recovery_scenarios" {
  // 测试错误恢复场景
  let normal_value = 42
  let error_value = -1
  
  // 模拟错误处理：如果出现错误值，使用默认值
  let input = error_value
  let safe_value = if input < 0 { normal_value } else { input }
  let result = add(safe_value, 10)
  
  assert_eq(52, result)
  
  // 测试断言函数在错误恢复中的应用
  assert_true(safe_value >= 0)
  assert_false(input >= 0)
}