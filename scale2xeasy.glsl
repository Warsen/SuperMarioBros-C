#if defined(VERTEX)

in vec4 VertexCoord;
in vec4 COLOR;
in vec4 TexCoord;
out vec4 COL0;
out vec4 TEX0;

uniform mat4 MVPMatrix;
uniform vec2 TextureSize;
out vec4 t1;
out vec4 t2;

void main()
{
    gl_Position = MVPMatrix * VertexCoord;
    COL0 = COLOR;
    TEX0.xy = TexCoord.xy;
	vec2 ps = vec2(vec4(TextureSize, 1.0 / TextureSize).z, vec4(TextureSize, 1.0 / TextureSize).w);
	float dx = ps.x;
	float dy = ps.y;

	t1 = TEX0.xyxy + vec4(  0,-dy,-dx,  0);	// B, D
	t2 = TEX0.xyxy + vec4( dx,  0,  0, dy);	// F, H	
}

#elif defined(FRAGMENT)

out vec4 FragColor;
uniform vec2 TextureSize;
uniform sampler2D Texture;
in vec4 TEX0;
in vec4 t1;
in vec4 t2;

bool eq(vec3 A, vec3 B){
	return (A==B);
}

bool neq(vec3 A, vec3 B){
	return (A!=B);
}

void main()
{
	// subpixel determination
	vec2 fp = floor(2.0 * fract(TEX0.xy * vec4(TextureSize, 1.0 / TextureSize).xy));

	/*
		  B		E0 E1
		D E F		E2 E3
		  H
	*/

	// reading the texels
	vec3 B = texture(Texture, t1.xy).xyz;
	vec3 D = texture(Texture, t1.zw).xyz;
	vec3 E = texture(Texture, TEX0.xy).xyz;
	vec3 F = texture(Texture, t2.xy).xyz;
	vec3 H = texture(Texture, t2.zw).xyz;

	// rules
	vec3 E0 = eq(B,D) ? B : E;
	vec3 E1 = eq(B,F) ? B : E;
	vec3 E2 = eq(H,D) ? H : E;
	vec3 E3 = eq(H,F) ? H : E;

	// general condition & subpixel output
	FragColor = vec4(neq(B,H) && neq(D,F) ? (fp.y == 0. ? (fp.x == 0. ? E0 : E1) : (fp.x == 0. ? E2 : E3)) : E, 1.0);
}

#endif
