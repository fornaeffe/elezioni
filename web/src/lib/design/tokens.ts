/**
 * Shared design tokens for all election UI pages.
 * This is the single source of truth for colors, spacing, typography, and sizing.
 * All extracted components use these tokens to ensure visual coherence.
 */

// ============================================================================
// COLORS
// ============================================================================

export const colors = {
  // Primary & semantic
  primary: '#245d73',          // Buttons, links, active states
  primaryHover: '#1d4659',     // Darker shade for hover/active states
  
  // Backgrounds & surfaces
  background: '#ffffff',       // Card/panel backgrounds
  surface: '#f4f7f8',          // Page background
  surfaceAlt: '#f1f6f8',       // Alternative surface (table headers, etc.)
  
  // Borders & dividers
  border: '#dce5e9',           // Standard panel/input borders
  borderLight: '#e3ebef',      // Lighter borders (subtle dividers)
  borderLighter: '#e5edf1',    // Very light borders (table rows)
  
  // Text
  textPrimary: '#182733',      // Main text
  textSecondary: '#60727e',    // Muted text (labels, descriptions)
  textMuted: '#637783',        // Even more muted (metadata, disabled)
  textInverse: '#ffffff',      // Text on dark backgrounds
  
  // States
  disabled: 'rgba(0,0,0,0.55)',
  disabledText: '#8a95a1',
  
  // Message states
  info: '#eef8fb',             // Info background
  infoText: '#49687a',         // Info text
  infoBorder: '#b7d4df',       // Info border
  
  warning: '#fff7df',          // Warning background
  warningText: '#5f5f2a',      // Warning text (fallback; adjust if needed)
  warningBorder: '#e4c785',    // Warning border
  
  error: '#ffeeee',            // Error background
  errorText: '#7f2a2a',        // Error text
  errorBorder: '#e8a8a8',      // Error border (approximate)
};

// ============================================================================
// SPACING
// ============================================================================

/**
 * Spacing scale in rem units.
 * Use consistently for padding, margins, and gaps.
 */
export const spacing = {
  xs: '0.25rem',    // 4px @ 16px base
  sm: '0.5rem',     // 8px
  md: '0.75rem',    // 12px
  lg: '1rem',       // 16px
  xl: '1.25rem',    // 20px
  xxl: '1.5rem',    // 24px
  xxxl: '2rem',     // 32px
};

// ============================================================================
// TYPOGRAPHY
// ============================================================================

export const typography = {
  fontFamily:
    'Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif',
  
  h1: {
    fontSize: '1.8rem',
    fontWeight: 'normal',
    letterSpacing: 0,
    lineHeight: '1.2',
  },
  h2: {
    fontSize: '1.3rem',
    fontWeight: 'normal',
    lineHeight: '1.2',
  },
  h3: {
    fontSize: '1rem',
    fontWeight: '700',
    lineHeight: '1.3',
  },
  body: {
    fontSize: '1rem',
    fontWeight: 'normal',
    lineHeight: '1.5',
  },
  bodySmall: {
    fontSize: '0.86rem',
    fontWeight: 'normal',
    lineHeight: '1.4',
  },
  label: {
    fontSize: '0.8rem',
    fontWeight: 'normal',
    lineHeight: '1.2',
  },
  labelSmall: {
    fontSize: '0.75rem',
    fontWeight: 'normal',
    lineHeight: '1.2',
  },
  monospace: {
    fontFamily: 'Monaco, "Courier New", monospace',
    fontSize: '0.85rem',
  },
};

// ============================================================================
// COMPONENT SIZING
// ============================================================================

export const sizing = {
  // Input/button heights for consistent vertical rhythm
  inputHeight: '2.2rem',
  buttonHeight: '2.2rem',
  
  // Color picker (narrower than inputs)
  colorPickerWidth: '2.5rem',
  colorPickerHeight: '2.2rem',
  
  // Icon sizing
  iconSmall: '16px',
  iconMedium: '20px',
  iconLarge: '24px',
  
  // Min widths for inputs (mobile-friendly)
  inputWidthNumber: '7rem',
  inputWidthShort: '9rem',
  inputWidthMedium: '12rem',
  inputWidthLong: '16rem',
};

// ============================================================================
// BORDER RADIUS
// ============================================================================

export const borderRadius = {
  small: '4px',
  medium: '6px',
  large: '8px',
};

// ============================================================================
// SHADOWS
// ============================================================================

export const shadows = {
  sm: '0 1px 2px rgba(0, 0, 0, 0.05)',
  md: '0 4px 6px rgba(0, 0, 0, 0.07)',
  lg: '0 10px 15px rgba(0, 0, 0, 0.1)',
};

// ============================================================================
// BREAKPOINTS
// ============================================================================

export const breakpoints = {
  mobile: '900px',
  tablet: '1200px',
};

// ============================================================================
// Z-INDEX SCALE
// ============================================================================

export const zIndex = {
  base: 0,
  dropdown: 100,
  modal: 1000,
  tooltip: 1100,
};

// ============================================================================
// ANIMATIONS & TRANSITIONS
// ============================================================================

export const transitions = {
  fast: '100ms ease-in-out',
  normal: '200ms ease-in-out',
  slow: '300ms ease-in-out',
};
