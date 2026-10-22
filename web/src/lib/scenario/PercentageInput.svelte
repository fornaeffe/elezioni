<script lang="ts">
  let { label, value, onchange, disabled = false, placeholder = 'Eredita', step = 'any' }: {
    label: string; value: number | undefined; onchange: (value: number | undefined) => void;
    disabled?: boolean; placeholder?: string; step?: string;
  } = $props();
</script>
<input type="number" min="0" max="100" {step} aria-label={label} {value} {disabled} {placeholder}
  oninput={(event) => onchange(event.currentTarget.value === '' ? undefined : event.currentTarget.valueAsNumber)} />
<style>input { font: inherit; padding: .5rem; border: 1px solid #a8bac6; border-radius: 4px; text-align: right; width: 100%; min-width: 0; box-sizing: border-box; }</style>
