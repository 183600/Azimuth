// 调试测试文件，用于验证边界情况

test "debug_add" {
  // 测试负数相加的边界情况
  assert_eq(-2147483648, add(-1073741824, -1073741824))?  // 应该溢出
  assert_eq(-2147483648, add(-1610612736, -536870912))?  // 应该溢出
  assert_eq(-2147483648, add(-2000000000, -147483648))?  // 应该溢出
  
  // 测试不溢出的负数相加
  assert_eq(-2147483647, add(-1073741824, -1073741823))?  // 不应该溢出
  assert_eq(-2, add(-1, -1))?  // 简单负数相加
  assert_eq(-2147483646, add(-2147483647, 1))?  // 负数加正数
  
  // 测试特殊情况
  assert_eq(-2147483647, add(-2147483648, 1))?  // 最小值 + 1
  assert_eq(-2147483647, add(1, -2147483648))?  // 1 + 最小值
  
  // 测试边界值
  assert_eq(2147483647, add(2147483646, 1))?  // 接近最大值
  assert_eq(2147483647, add(2147483647, 1))?  // 溢出
  assert_eq(2147483647, add(2147483645, 3))?  // 溢出
  
  // 测试0的情况
  assert_eq(0, add(0, 0))?
  assert_eq(5, add(0, 5))?
  assert_eq(-5, add(-5, 0))?
  
  // 测试正负数混合
  assert_eq(-1, add(2147483647, -2147483648))?  // 最大值 + 最小值
  assert_eq(0, add(2147483647, -2147483647))?  // 最大值 + -最大值
  assert_eq(100, add(200, -100))?  // 普通正负数相加
}

test "debug_multiply" {
  // 测试最小值的特殊情况
  assert_eq(-2147483648, multiply(-2147483648, 1))?  // 最小值乘1
  assert_eq(-2147483648, multiply(1, -2147483648))?  // 1乘最小值
  assert_eq(-2147483648, multiply(-2147483648, -1))?  // 最小值乘-1（溢出）
  assert_eq(-2147483648, multiply(-1, -2147483648))?  // -1乘最小值（溢出）
  
  // 测试最小值乘以大于1的数
  assert_eq(-2147483648, multiply(-2147483648, 2))?  // 最小值乘2
  assert_eq(-2147483648, multiply(-2147483648, -2))?  // 最小值乘-2
  assert_eq(-2147483648, multiply(2, -2147483648))?  // 正数乘以最小值
  assert_eq(-2147483648, multiply(-2, -2147483648))?  // 负数乘以最小值
  
  // 测试0的情况
  assert_eq(0, multiply(0, 0))?
  assert_eq(0, multiply(0, 5))?
  assert_eq(0, multiply(-5, 0))?
  assert_eq(0, multiply(0, -2147483648))?  // 0乘以最小值
  assert_eq(0, multiply(-2147483648, 0))?  // 最小值乘以0
  
  // 测试1的情况
  assert_eq(5, multiply(1, 5))?
  assert_eq(-5, multiply(1, -5))?
  assert_eq(5, multiply(5, 1))?
  assert_eq(-5, multiply(-5, 1))?
  
  // 测试-1的情况
  assert_eq(-5, multiply(-1, 5))?
  assert_eq(5, multiply(-1, -5))?
  assert_eq(-5, multiply(5, -1))?
  assert_eq(5, multiply(-5, -1))?
  
  // 测试边界溢出
  assert_eq(2147483647, multiply(46341, 46341))?  // 应该溢出
  assert_eq(2147483647, multiply(46342, 46342))?  // 应该溢出
  assert_eq(2147483647, multiply(65536, 65536))?  // 65536*65536=4294967296，应该溢出
  
  // 测试负溢出
  assert_eq(-2147483648, multiply(-46341, 46341))?  // 应该负溢出
  assert_eq(-2147483648, multiply(46341, -46341))?  // 应该负溢出
  
  // 测试边界值
  assert_eq(2147483647, multiply(2147483647, 1))?  // 最大值乘1
  assert_eq(-2147483647, multiply(2147483647, -1))?  // 最大值乘-1
  assert_eq(-2147483647, multiply(-2147483647, 1))?  // -2147483647乘1
  assert_eq(2147483647, multiply(-2147483647, -1))?  // -2147483647乘-1
}

test "specific_add_test" { 
  assert_eq(-4, add(-2, -2))? 
}

test "multiply_zero_test" { 
  assert_eq(0, multiply(0, 0))? 
}

test "add_edge_case" { 
  assert_eq(-2147483648, add(-1073741824, -1073741824))? 
}

test "add_min_value_positive" {
  assert_eq(-2147483647, add(-2147483648, 1))?  // 最小值 + 1
  assert_eq(-2147483647, add(1, -2147483648))?  // 1 + 最小值
}
