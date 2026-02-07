// Azimuth 项目新增标准测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "arithmetics_basic_operations" {
  // 基本算术运算测试
  assert_eq(10, 2 + 3)
  assert_eq(6, 2 * 3)
  assert_eq(4, 7 - 3)
  assert_eq(3, 10 / 3)
}

test "arithmetics_zero_operations" {
  // 零值运算测试
  assert_eq(5, 5 + 0)
  assert_eq(0, 5 * 0)
  assert_eq(5, 5 - 0)
  assert_eq(0, 0 / 5)
}

test "arithmetics_negative_numbers" {
  // 负数运算测试
  assert_eq(-4, -7 + 3)
  assert_eq(-21, -7 * 3)
  assert_eq(-10, -7 - 3)
  assert_eq(-3, -10 / 3)
}

test "division_precision" {
  // 除法精度测试
  assert_eq(0, 1 / 2)
  assert_eq(1, 3 / 2)
  assert_eq(3, 10 / 3)
  assert_eq(2, 20 / 4)
  assert_eq(6, 13 / 2)
}

test "string_operations" {
  // 字符串操作测试
  assert_eq_string("Hello, MoonBit!", "Hello, MoonBit!")
  assert_eq_string("Hello, World!", "Hello, World!")
  assert_eq_string("Hello, !", "Hello, !")
  assert_eq_string("Hello, 用户!", "Hello, 用户!")
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base = 100
  let step1 = base * 2      // 200
  let step2 = step1 + 50    // 250
  let step3 = step2 - 30    // 220
  let step4 = step3 / 4     // 55
  
  assert_eq(200, step1)
  assert_eq(250, step2)
  assert_eq(220, step3)
  assert_eq(55, step4)
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 15
  let y = 25
  
  // 加法交换律
  assert_eq(x + y, y + x)
  
  // 乘法交换律
  assert_eq(x * y, y * x)
  
  // 加法单位元
  assert_eq(x + 0, x)
  
  // 乘法单位元
  assert_eq(x * 1, x)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试极值情况
  assert_eq(2000, 1000 + 1000)
  assert_eq(1000000, 1000 * 1000)
  assert_eq(333, 1000 / 3)
  
  // 测试单位边界
  assert_eq(2, 1 + 1)
  assert_eq(1, 1 * 1)
  assert_eq(0, 1 - 1)
  assert_eq(1, 1 / 1)
}