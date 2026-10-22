const safeColor = (value: string) => /^#[0-9a-f]{6}$/i.test(value) ? value : '#5f7f92';

function parseHexColor(color: string): [number, number, number] {
  const safe = safeColor(color).slice(1);
  return [0, 2, 4].map((index) => Number.parseInt(safe.slice(index, index + 2), 16)) as [number, number, number];
}

function toHex(value: number): string {
  return Math.round(Math.max(0, Math.min(255, value)))
    .toString(16)
    .padStart(2, '0');
}

function mixColor(left: string, right: string, amount: number): string {
  const l = parseHexColor(left);
  const r = parseHexColor(right);
  const mixed = l.map((value, index) => value + (r[index] - value) * amount);
  return `#${mixed.map(toHex).join('')}`;
}

export function readableTextColor(backgroundColor: string): string {
  const [red, green, blue] = parseHexColor(backgroundColor).map((value) => {
    const normalized = value / 255;
    return normalized <= 0.03928 ? normalized / 12.92 : ((normalized + 0.055) / 1.055) ** 2.4;
  });
  const luminance = 0.2126 * red + 0.7152 * green + 0.0722 * blue;
  return luminance < 0.42 ? '#ffffff' : '#182026';
}

export function spineColor(baseColor: string, index: number, total: number): string {
  if (total <= 1) return mixColor(baseColor, '#ffffff', 0.35);
  const amount = index / (total - 1);
  return amount < 0.5
    ? mixColor('#000000', baseColor, amount * 2)
    : mixColor(baseColor, '#ffffff', (amount - 0.5) * 2);
}

