import { base } from '$app/paths';
import { goto } from '$app/navigation';
import { transitionalFileWorkspace as fileWorkspace, transitionalScenarioWorkspace as scenarioWorkspace } from './import-routing';
import { queueWorkspaceImport, type WorkspaceImport } from './handoff';

export async function navigateWorkspaceImport(input: WorkspaceImport) {
  const target = input.file ? await fileWorkspace(input.file) : scenarioWorkspace(input.scenario ?? input.payload?.scenario);
  queueWorkspaceImport(target.id, input);
  await goto(`${base}${target.route}`);
}
