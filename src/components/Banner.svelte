<script>
  export let type = 'phase'; // or 'note'
  export let text = '';
  
  const getBannerClass = (type) => {
    return type === 'phase' ? 'phase-banner' : 'note-banner';
  };
</script>

<span class={getBannerClass(type)}>{text}</span>

<style>
  .phase-banner, .note-banner {
    color: white;
    font-weight: 700;
    font-size: 1rem;
    letter-spacing: 1px;
    padding: 5px 8px 4px;
    margin-right: 10px;
    border-radius: 3px;
    display: inline-block;
  }

  .phase-banner {
    background-color: #6c757d;
  }

  .note-banner {
    background-color: #ffc008;
  }
</style>