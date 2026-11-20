export const dynamic = 'force-dynamic';

export async function GET(request, { params }) {
  const { id } = await params;
  return new Response(id);
}
