float4 pixel(float4 input:POSITION) : SV_TARGET
{
	int x = 0;
	{
		int y = 5;
		x = y;
	}
	int y = 7;
	int z = x + y;
	return float4(1.0f,1.0f,1.0f,1.0f);
}