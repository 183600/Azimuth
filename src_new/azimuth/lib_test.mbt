// 基础测试文件

test "add_functions" {
  // 基础功能测试
  assert_eq(3, add(1, 2))
  assert_eq(0, add(-1, 1))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(0, 0))
  
  // 正数溢出测试
  assert_eq(2147483647, add(2147483646, 1))// 接近Int最大值
  assert_eq(2147483647, add(2147483647, 0))// 最大值加0
  assert_eq(2147483647, add(2147483647, 1))// 溢出情况
  assert_eq(2147483647, add(1000000000, 2000000000))// 溢出情况
  assert_eq(2147483647, add(2147483645, 3))// 2147483645 + 3 = 2147483648，应该溢出
  
  // 负数溢出测试
  assert_eq(-2147483648, add(-2147483648, 0))// 最小值加0
  assert_eq(-2147483648, add(-2147483648, -1))// 溢出情况
  assert_eq(-2147483648, add(-2147483648, -1000000000))// 溢出情况
  assert_eq(-2147483648, add(-2147483647, -1))// 边界负数相加
  assert_eq(-2147483648, add(-2147483647, -2))// 边界负数相加
  assert_eq(-2147483648, add(-2000000000, -2000000000))// 大负数相加
  assert_eq(-2147483648, add(-1073741824, -1073741824))// 应该溢出
  assert_eq(-2147483648, add(-1610612736, -536870912))// 应该溢出
  assert_eq(-2147483648, add(-2000000000, -147483648))// 应该溢出
  
  // 不溢出的负数相加
  assert_eq(-2147483647, add(-1073741824, -1073741823))// 不应该溢出
  assert_eq(-2, add(-1, -1))// 简单负数相加
  assert_eq(-2147483646, add(-2147483647, 1))// 负数加正数
  assert_eq(-2147483647, add(-2147483646, -1))// 不溢出
  assert_eq(-100, add(-50, -50))// 普通负数相加
  
  // 正负数混合相加测试
  assert_eq(-1, add(2147483647, -2147483648))// 最大值 + 最小值
  assert_eq(-1, add(-2147483648, 2147483647))// 最小值 + 最大值
  assert_eq(-2147483647, add(1, -2147483648))// 1 + 最小值
  assert_eq(-2147483647, add(-2147483648, 1))// 最小值 + 1
  assert_eq(2147483646, add(2147483647, -1))// 最大值 - 1
  assert_eq(100, add(200, -100))// 普通正负数相加
  assert_eq(-100, add(-200, 100))// 普通负正数相加
  assert_eq(0, add(2147483647, -2147483647))// 最大值 + -最大值 = 0
  
  // 综合边界测试
  assert_eq(2147483647, add(1073741824, 1073741824))
  assert_eq(-2147483648, add(-2147483646, -3))
}

test "multiply_functions" {
  // 基础功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
  assert_eq(1, multiply(1, 1))
  assert_eq(0, multiply(0, 0))
  assert_eq(1, multiply(-1, -1))
  
  // 最小值相关测试
  assert_eq(-2147483648, multiply(-2147483648, 1))// 最小值乘1
  assert_eq(-2147483648, multiply(1, -2147483648))// 1乘最小值
  assert_eq(-2147483648, multiply(-2147483648, -1))// 最小值乘-1（溢出）
  assert_eq(-2147483648, multiply(-1, -2147483648))// -1乘最小值（溢出）
  assert_eq(0, multiply(-2147483648, 0))// 最小值乘0
  assert_eq(0, multiply(0, -2147483648))// 0乘最小值
  
  // 最小值乘以大于1的数
  assert_eq(-2147483648, multiply(-2147483648, 2))// 最小值乘2
  assert_eq(-2147483648, multiply(-2147483648, -2))// 最小值乘-2
  assert_eq(-2147483648, multiply(2, -2147483648))// 正数乘以最小值
  assert_eq(-2147483648, multiply(-2, -2147483648))// 负数乘以最小值
  
  // 边界值测试
  assert_eq(2147483647, multiply(2147483647, 1))// 最大值乘1
  assert_eq(-2147483647, multiply(2147483647, -1))// 最大值乘-1
  assert_eq(-2147483647, multiply(-2147483647, 1))// 接近最小值乘1
  assert_eq(2147483647, multiply(-2147483647, -1))// 接近最小值乘-1
  
  // 溢出测试
  assert_eq(2147483647, multiply(46341, 46341))// 接近溢出的平方根
  assert_eq(2147483647, multiply(46342, 46342))// 正溢出
  assert_eq(2147483647, multiply(50000, 50000))// 正溢出
  assert_eq(2147483647, multiply(100000, 30000))// 溢出情况
  assert_eq(2147483647, multiply(65536, 65536))// 65536*65536=4294967296，应该溢出
  assert_eq(2147441940, multiply(46340, 46341))// 接近溢出
  assert_eq(2147441940, multiply(46341, 46340))// 接近溢出
  
  // 负溢出测试
  assert_eq(-2147483648, multiply(-100000, 30000))// 负溢出情况
  assert_eq(-2147483648, multiply(-46341, 46341))// 负溢出
  assert_eq(-2147483648, multiply(46341, -46341))// 负溢出
}

test "greet_function" {
  assert_eq("Hello, World!", greet("World"))
  assert_eq("Hello, MoonBit!", greet("MoonBit"))
  // 边界情况测试
  assert_eq("Hello, !", greet(""))// 空字符串
  assert_eq("Hello, A!", greet("A"))// 单字符
  assert_eq("Hello, 123!", greet("123"))// 数字字符串
}