// 简单测试验证文件
// 包含1个基本测试用例

test "simple_addition_test" {
  // 使用 lib.mbt 中定义的函数
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}