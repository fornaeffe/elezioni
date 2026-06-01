import type { Rng } from './rng';
import { createSeededRng } from './rng';

export interface HareNiemeyerDetails {
  assigned: number[];
  remainders: number[];
  remainderSeats: number[];
}

interface RankedRemainder {
  index: number;
  votes: number;
  remainder: number;
  draw: number;
}

const defaultRng = createSeededRng('allocation-default');

function sum(values: readonly number[]): number {
  return values.reduce((total, value) => total + value, 0);
}

function assertVector(values: readonly number[], name: string): void {
  if (values.length === 0) {
    throw new Error(`${name} must not be empty`);
  }

  for (const value of values) {
    if (!Number.isFinite(value) || value < 0) {
      throw new Error(`${name} must contain only finite non-negative numbers`);
    }
  }
}

function normalizeMinimums(minimums: number | readonly number[], length: number): number[] {
  if (typeof minimums === 'number') {
    return Array.from({ length }, () => minimums);
  }

  if (minimums.length !== length) {
    throw new Error('minimums must be a scalar or match the votes length');
  }

  return [...minimums];
}

export function hareNiemeyerDetails(
  votes: readonly number[],
  seats: number,
  rng: Rng = defaultRng
): HareNiemeyerDetails {
  assertVector(votes, 'votes');

  if (!Number.isInteger(seats) || seats < 0) {
    throw new Error('seats must be a non-negative integer');
  }

  if (seats === 0) {
    return {
      assigned: Array.from({ length: votes.length }, () => 0),
      remainders: [...votes],
      remainderSeats: Array.from({ length: votes.length }, () => 0)
    };
  }

  const totalVotes = sum(votes);

  if (totalVotes === 0) {
    return {
      assigned: Array.from({ length: votes.length }, () => 0),
      remainders: Array.from({ length: votes.length }, () => 0),
      remainderSeats: Array.from({ length: votes.length }, () => 0)
    };
  }

  const quota = totalVotes / seats;
  const assigned = votes.map((vote) => Math.floor(vote / quota));
  const remainders = votes.map((vote, index) => vote - assigned[index] * quota);
  const stillToAssign = seats - sum(assigned);
  const ranked: RankedRemainder[] = votes.map((vote, index) => ({
    index,
    votes: vote,
    remainder: remainders[index],
    draw: rng()
  }));

  ranked.sort((left, right) => {
    if (right.remainder !== left.remainder) return right.remainder - left.remainder;
    if (right.votes !== left.votes) return right.votes - left.votes;
    return left.draw - right.draw;
  });

  const remainderSeats = Array.from({ length: votes.length }, () => 0);
  for (const row of ranked.slice(0, stillToAssign)) {
    remainderSeats[row.index] = 1;
    assigned[row.index] += 1;
  }

  return { assigned, remainders, remainderSeats };
}

export function hareNiemeyer(
  votes: readonly number[],
  seats: number,
  rng: Rng = defaultRng
): number[] {
  return hareNiemeyerDetails(votes, seats, rng).assigned;
}

export function hareNiemeyerWithMinimum(
  votes: readonly number[],
  seats: number,
  minimums: number | readonly number[],
  rng: Rng = defaultRng
): number[] {
  assertVector(votes, 'votes');
  const normalizedMinimums = normalizeMinimums(minimums, votes.length);
  assertVector(normalizedMinimums, 'minimums');

  const minimumTotal = sum(normalizedMinimums);
  if (minimumTotal > seats) {
    throw new Error('minimums exceed seats');
  }

  const quota = sum(votes) / seats;
  const adjustedVotes = votes.map((vote, index) => Math.max(vote - quota * normalizedMinimums[index], 0));
  const variableSeats = hareNiemeyer(adjustedVotes, seats - minimumTotal, rng);

  return normalizedMinimums.map((minimum, index) => minimum + variableSeats[index]);
}

interface DHondtQuotient {
  index: number;
  votes: number;
  quotient: number;
  draw: number;
}

export function dHondt(
  votes: readonly number[],
  seats: number,
  maxCandidates: readonly number[] = Array.from({ length: votes.length }, () => seats),
  rng: Rng = defaultRng
): number[] {
  assertVector(votes, 'votes');
  assertVector(maxCandidates, 'maxCandidates');

  if (!Number.isInteger(seats) || seats < 0) {
    throw new Error('seats must be a non-negative integer');
  }

  if (maxCandidates.length !== votes.length) {
    throw new Error('maxCandidates must match the votes length');
  }

  const quotients: DHondtQuotient[] = [];
  for (let index = 0; index < votes.length; index += 1) {
    const usableCandidates = Math.min(seats, Math.floor(maxCandidates[index]));
    for (let divisor = 1; divisor <= usableCandidates; divisor += 1) {
      quotients.push({
        index,
        votes: votes[index],
        quotient: votes[index] / divisor,
        draw: rng()
      });
    }
  }

  quotients.sort((left, right) => {
    if (right.quotient !== left.quotient) return right.quotient - left.quotient;
    if (right.votes !== left.votes) return right.votes - left.votes;
    return left.draw - right.draw;
  });

  const assigned = Array.from({ length: votes.length }, () => 0);
  for (const quotient of quotients.slice(0, seats)) {
    assigned[quotient.index] += 1;
  }

  return assigned;
}
