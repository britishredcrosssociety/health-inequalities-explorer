<script>
  import { onMount } from 'svelte';
  import HelpButton from './HelpButton.svelte';
  import Banner from './Banner.svelte';
  import { fade } from 'svelte/transition';
  
  export let selectedRegion;
  export let selectedGeography;
  export let selectedAreas = [];
  
  let isDetailsCollapsed = true;
  let plotInstance;
  let metricsData = [];

  // This would typically come from your data store or API
  let indicatorDescriptions = [
    {
      title: "Life Expectancy",
      description: "Average number of years a person can expect to live based on current mortality rates...",
      source: "ONS, 2020-22",
      methodology: "Life expectancy represents the average number of years a person would live..."
    },
    // Add other indicator descriptions
  ];

  onMount(async () => {
    // Do nothing for now
  });

  function toggleDetails() {
    isDetailsCollapsed = !isDetailsCollapsed;
  }
</script>

<section class="summary-metrics">
  <div class="header">
    <h4><b>Summary Indicators</b></h4>
    <Banner type="note" text="NOTE" />
    <p>Clusters of points have similar values. See the help button for more info.</p>
  </div>

  <div class="plot-container">
    <div id="summaryMetricsPlot" class="plot"></div>
    <div class="help-button-container">
      <HelpButton onClick={() => alert('Show help modal')} />
    </div>
  </div>

  <div class="indicator-details">
    <button 
      class="details-toggle" 
      on:click={toggleDetails}
      aria-expanded={!isDetailsCollapsed}
    >
      <span>Show indicator details</span>
      <span class="arrow" class:rotated={!isDetailsCollapsed}>▼</span>
    </button>

    {#if !isDetailsCollapsed}
      <div class="details-content" transition:fade>
        {#each indicatorDescriptions as indicator}
          <div class="indicator">
            <h5>{indicator.title}</h5>
            <p>{indicator.description}</p>
            <div class="metadata">
              <span class="source">Source: {indicator.source}</span>
              <details>
                <summary>Methodology</summary>
                <p>{indicator.methodology}</p>
              </details>
            </div>
          </div>
        {/each}
      </div>
    {/if}
  </div>
</section>

<style>
  .summary-metrics {
    grid-area: metrics;
    padding: 1rem;
    background-color: white;
  }

  .header {
    margin-bottom: 1rem;
  }

  .header h4 {
    margin: 0;
    color: #193351;
  }

  .plot-container {
    position: relative;
    width: 100%;
    height: 500px;
    margin: 1rem 0;
  }

  .plot {
    width: 100%;
    height: 100%;
  }

  .help-button-container {
    position: absolute;
    top: 0;
    right: 0;
  }

  .indicator-details {
    margin-top: 1rem;
  }

  .details-toggle {
    width: 100%;
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.5rem;
    background: none;
    border: none;
    color: #1d70b8;
    text-decoration: underline;
    cursor: pointer;
  }

  .details-toggle:hover {
    color: #102f73;
  }

  .arrow {
    transition: transform 0.3s ease;
  }

  .arrow.rotated {
    transform: rotate(180deg);
  }

  .details-content {
    padding: 1rem;
    border-top: 1px solid #e5e5e5;
  }

  .indicator {
    margin-bottom: 2rem;
  }

  .indicator:last-child {
    margin-bottom: 0;
  }

  .indicator h5 {
    margin: 0 0 0.5rem 0;
    color: #193351;
  }

  .metadata {
    margin-top: 0.5rem;
    font-size: 0.9rem;
    color: #666;
  }

  .source {
    display: block;
    margin-bottom: 0.5rem;
  }

  details summary {
    cursor: pointer;
    color: #1d70b8;
  }

  details summary:hover {
    color: #102f73;
  }
</style>