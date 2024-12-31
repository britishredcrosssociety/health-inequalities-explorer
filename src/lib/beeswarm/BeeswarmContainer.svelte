<script lang="ts">
	import Beeswarm from './Beeswarm.svelte';
	import { scaleLinear } from 'd3-scale';

	export let metadata,
		indicator,
		selectionsObject,
		selectedArea,
		hoverAreaId,
		hoverIndicatorId,
		selectedFilteredChartDataBeeswarmWithRole,
		comparisonFilteredChartDataBeeswarmWithRole,
		additionalFilteredChartDataBeeswarm,
		filteredChartDataBeeswarm,
		showConfidenceIntervals,
		customLookup,
		indicatorCalculations;
	
	const indicatorRowBeeswarmChartSettings = {
		height: 80,
		padding: { top: 45, right: 0, bottom: 5, left: 0 },
		spaceForOutliers: 40,
		defaultMadRange: 3,
		geographyLevelWhereComparisonTextIsProvided: ['upper', 'lower'],
		minimumAreasForComparisonTextToBeProvided: 10,
		primaryCircleRadius: 8,
		backgroundCircleRadius: { 20: 6.4, 50: 6, 100: 5.5, 200: 5, default: 5 }
	};

	//set initial width of svg, this is used for the initial render but then the screen size is determined and the width of the svg is re-calculated.
	let width = 1000;

	let height = indicatorRowBeeswarmChartSettings.height;
	let padding = indicatorRowBeeswarmChartSettings.padding;
	let spaceForOutliers = indicatorRowBeeswarmChartSettings.spaceForOutliers;

	$: chartWidth = width - padding.left - padding.right - spaceForOutliers * 2;
	$: chartHeight = height - padding.top - padding.bottom;

	//the mad range is used to set the scale for the chart. so for example, if the madRange is set to 1, the chart range will go from -1 mad from the median to 1 mad from the median. any areas which do not fall within this range will be stacked as outliers and the beginning/end of the chart
	$: madRange =
		indicator.metadata.beeswarmRowUseMinMax === 'T'
			? 'minMax'
			: indicatorRowBeeswarmChartSettings.defaultMadRange;

	//if madRange is minMax, the scale is simply set based on the range of the data points. this is determined based on the indicators metadata and used for certain indicators where the value are very bunched together, such as 4G coverage. in this instance, the furthestDistanceFromMedian is used to set the scale
	$: furtherDistanceFromMedian =
		madRange != 'minMax'
			? null
			: Math.max(
					indicatorCalculations.med - indicatorCalculations.min,
					indicatorCalculations.max - indicatorCalculations.med
				);

	$: xDomain =
		madRange != 'minMax'
			? [
					indicatorCalculations.med - madRange * indicatorCalculations.mad,
					indicatorCalculations.med + madRange * indicatorCalculations.mad
				]
			: [
					indicatorCalculations.med - furtherDistanceFromMedian,
					indicatorCalculations.med + furtherDistanceFromMedian
				];

	$: x = scaleLinear().domain(xDomain).range([0, chartWidth]);

	$: beeswarmAltText =
		`Beeswarm chart showing values for ${indicator.metadata.label}` +
		(indicator.metadata?.subText ? ` (${indicator.metadata?.subText}). ` : `. `) +
		`The value for ${selectedArea.areanm} was ${indicator.metadata?.prefix}${roundNumber(selectedFilteredChartDataBeeswarmWithRole?.value, indicator.metadata.decimalPlaces)}${indicator.metadata?.suffix}, ` +
		(comparisonFilteredChartDataBeeswarmWithRole?.value
			? `The ${selectionsObject['areas-rows-comparison-visible']?.label || selectionsObject['areas-rows-comparison-visible']?.areanm} value was ${indicator.metadata?.prefix}${roundNumber(comparisonFilteredChartDataBeeswarmWithRole.value, indicator.metadata.decimalPlaces)}${indicator.metadata?.suffix}.`
			: `No value available for ${selectionsObject['areas-rows-comparison-visible']?.label || selectionsObject['areas-rows-comparison-visible']?.areanm}.`);

	$: console.log(beeswarmAltText);
</script>

<figure class="beeswarm-figure">
	<div class="svg-container" bind:clientWidth={width}>
		<svg
			role="img"
			aria-labelledby={indicator.metadata.slug + '-beeswarm-description'}
			{width}
			{height}
		>
			<desc id={indicator.metadata.slug + '-beeswarm-description'}>{beeswarmAltText}</desc>
			{#if chartWidth && chartHeight}
				<g
					aria-hidden="true"
					transform="translate({padding.left + spaceForOutliers},{padding.top})"
				>
					{#if chartWidth && chartHeight}
						<Beeswarm
							{metadata}
							{indicator}
							{selectedFilteredChartDataBeeswarmWithRole}
							{comparisonFilteredChartDataBeeswarmWithRole}
							{additionalFilteredChartDataBeeswarm}
							{filteredChartDataBeeswarm}
							{selectedArea}
							{selectionsObject}
							bind:hoverAreaId
							bind:hoverIndicatorId
							{spaceForOutliers}
							{chartWidth}
							{chartHeight}
							{xDomain}
							{customLookup}
							{showConfidenceIntervals}
							{width}
							{x}
						></Beeswarm>
					{/if}
				</g>
			{/if}
		</svg>
	</div>
</figure>

<style>
	.beeswarm-figure {
		margin: 0;
	}

	svg {
		overflow: visible;
		padding: 0px;
		margin: 0px;
	}
</style>
