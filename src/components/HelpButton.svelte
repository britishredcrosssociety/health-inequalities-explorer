<script>
  export let onClick = () => {};
</script>

<button
  class="help-button"
  on:click={onClick}
  aria-label="Show help information"
>
  ?
</button>

<style>
  .help-button {
    font-weight: 700;
    background-color: #f8f9fa;
    border: 1px solid #E0E0E0;
    border-radius: 100%;
    padding: 9px 18px;
    cursor: pointer;
    transition: all 0.2s ease;
  }

  .help-button:hover {
    background-color: #f8f9fa;
    border-color: #717171;
    box-shadow: rgba(0, 0, 0, .1) 0 2px 1px;
  }

  .help-button:active {
    outline: none;
    transform: translateY(2px);
  }

  .help-button:focus {
    outline: 2px solid #1d70b8;
    outline-offset: 2px;
  }
</style>